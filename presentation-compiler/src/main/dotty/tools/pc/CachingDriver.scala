package dotty.tools.pc

import scala.meta.internal.pc.*

import java.net.URI
import java.nio.file.Path
import java.util as ju

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.meta.pc.SemanticdbFileManager
import scala.meta.pc.SourcePathMode

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.{
  CachedLogicalPackage,
  InteractiveDriver,
  LogicalPackagesProvider,
  ParsedLogicalPackage
}
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Comments
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.config.JavaPlatform
import dotty.tools.dotc.config.Platform
import dotty.tools.dotc.config.SJSPlatform
import dotty.tools.dotc.classpath.AggregateClassPath
import dotty.tools.dotc.classpath.BinaryFileEntry
import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.dotc.classpath.PackageNameUtils
import dotty.tools.dotc.classpath.SourceFileEntry
import java.io.File
import java.nio.file.Paths
import java.nio.file.Path
import java.net.URL
import dotty.tools.io.AbstractFile
import dotty.tools.io.metals.Archives
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.function.Consumer
import dotty.tools.dotc.reporting.Reporter

/** CachingDriver is a wrapper class that provides a compilation cache for
 *  InteractiveDriver. CachingDriver skips running compilation if
 *    - the target URI of `run` is the same as the previous target URI
 *    - the content didn't change since the last compilation.
 *
 *  This compilation cache enables Metals to skip compilation and re-use the
 *  typed tree under the situation like developers sequentially hover on the
 *  symbols in the same file without any changes.
 *
 *  Note: we decided to cache only if the target URI is the same as in the
 *  previous run because of `InteractiveDriver.currentCtx` that should return
 *  the context that refers to the last compiled source file. It would be ideal
 *  if we could update currentCtx even when we skip the compilation, but we
 *  struggled to do that. See the discussion
 *  https://github.com/scalameta/metals/pull/4225#discussion_r941138403 To avoid
 *  the complexity related to currentCtx, we decided to cache only when the
 *  target URI only if the same as the previous run.
 */
class CachingDriver private (
    override val settings: List[String],
    sourcePackage: CachedLogicalPackage,
    compilerAccess: Scala3CompilerAccess,
    userLogger: Consumer[String],
    emitDiagnostics: Consumer[(URI, Seq[Diagnostic])]
) extends InteractiveDriver(settings, sourcePackage):

  private var lastCompiledURI: URI = uninitialized
  private var previousDiags = List.empty[Diagnostic]

  override protected def initCtx: Context = {
    val baseCtx: ContextBase = new ContextBase { baseCtx0 =>
      override protected def newPlatform(using Context): Platform = {
        if (baseCtx0.settings.scalajs.value) new SJSPlatform(sourcePackage.value)
        else
          new JavaPlatform(sourcePackage.value) {
            override def classPath(using Context): ClassPath = {

              val modFiles = baseCtx0.settings.classpath.value
                .split(File.pathSeparator)
                .filter(_.endsWith(".jmod"))
                .map(Paths.get(_))

              lazy val modCp: ClassPath = new ClassPath {
                import CachingDriver.fza

                def cpIterator(): Iterator[Path] =
                  modFiles.iterator

                private def packageDir(archive: AbstractFile, pkg: String): Option[AbstractFile] = {
                  val dirName = ("classes" +: pkg.split('.').filter(_.nonEmpty)).mkString("/")
                  Option(archive.lookupName(dirName, directory = true))
                }

                override def asURLs: Seq[URL] =
                  cpIterator()
                    .map(_.toUri.toURL)
                    .toVector

                override def findClassFile(className: String): Option[AbstractFile] = {
                  val (pkg, simpleName) = PackageNameUtils.separatePkgAndClassNames(className)
                  cpIterator()
                    .flatMap(f => packageDir(fza(f), pkg).iterator)
                    .flatMap(dir => Option(dir.lookupName(simpleName + ".class", directory = false)).iterator)
                    .take(1)
                    .toList
                    .headOption
                }

                override def hasPackage(pkg: String): Boolean =
                  cpIterator().exists(f => packageDir(fza(f), pkg).nonEmpty)

                override def packages(inPackage: String): Iterable[String] = {
                  val prefix = PackageNameUtils.packagePrefix(inPackage)
                  cpIterator()
                    .flatMap(f => packageDir(fza(f), inPackage).iterator)
                    .flatMap(_.iterator)
                    .filter(_.isDirectory)
                    .map(prefix + _.name)
                    .toVector
                    .distinct
                }
                override def classes(inPackage: String): Iterable[BinaryFileEntry] =
                  cpIterator()
                    .flatMap(f => packageDir(fza(f), inPackage).iterator)
                    .flatMap(_.iterator)
                    .filter(!_.isDirectory)
                    .filter(_.name.endsWith(".class"))
                    .map(BinaryFileEntry(_))
                    .toVector
                    .distinct
                // Listing the JDK sources would make the compiler load JDK classes from them:
                // SymbolLoaders prefers sources at least as recent as their class files,
                // which is all of them in JDKs with reproducible timestamps
                override def sources(inPackage: String): Iterable[SourceFileEntry] =
                  Nil
              }

              def process(cp: ClassPath): Option[ClassPath] =
                cp match {
                  case agg: AggregateClassPath =>
                    val res = agg.aggregates.flatMap(cp0 => process(cp0).toSeq)
                    if (res.isEmpty) None
                    else Some(AggregateClassPath(res))
                  case _: dotty.tools.dotc.classpath.JrtClassPath =>
                    None
                  case other =>
                    Some(other)
                }

              val cp = super.classPath
              process(cp).getOrElse(AggregateClassPath(Nil)) match {
                case agg: AggregateClassPath =>
                  AggregateClassPath(modCp +: agg.aggregates)
                case other =>
                  AggregateClassPath(Seq(modCp, other))
              }
            }
          }
      }
    }
    baseCtx.initialCtx
      .withProperty(Comments.ContextDoc, Some(new Comments.ContextDocstrings))
      .fresh
      .setReporter(
        new Reporter {
          def doReport(dia: Diagnostic)(using Context): Unit = {
            userLogger.accept(dia.toString)
          }
        }
      )
  }

  private def alreadyCompiled(uri: URI, content: Array[Char]): Boolean =
    compilationUnits.get(uri) match
      case Some(unit)
          if lastCompiledURI == uri &&
            ju.Arrays.equals(unit.source.content(), content) =>
        true
      case _ => false

  override def run(uri: URI, source: SourceFile): List[Diagnostic] =
    if (!alreadyCompiled(uri, source.content)) {
      val id =
        if (compilerAccess.beforeAccessOpt.nonEmpty || compilerAccess.afterAccessOpt.nonEmpty)
          UUID.randomUUID().toString
        else
          ""
      for (f <- compilerAccess.beforeAccessOpt)
        f(id, "typechecking", uri.toASCIIString)
      try {
        previousDiags = super.run(uri, source)
        emitDiagnostics.accept((uri, previousDiags))
      }
      finally {
        for (f <- compilerAccess.afterAccessOpt)
          f(id, "typechecking", uri.toASCIIString)
      }
    }
    lastCompiledURI = uri
    previousDiags

  def freshDriver(): InteractiveDriver =
    new CachingDriver(settings, sourcePackage, compilerAccess, userLogger, emitDiagnostics)

end CachingDriver

object CachingDriver:
  def apply(
      settings: List[String],
      sourcePath: ju.function.Supplier[ju.List[Path]],
      semanticdbFileManager: SemanticdbFileManager,
      sourcePathMode: SourcePathMode,
      compilerAccess: Scala3CompilerAccess,
      userLogger: Consumer[String],
      emitDiagnostics: Consumer[(URI, Seq[Diagnostic])]
  ): CachingDriver =
    def sourcePackagesExtractor(using Context) = sourcePathMode match
      case SourcePathMode.DISABLED => None
      case SourcePathMode.PRUNED | SourcePathMode.FULL =>
        val sourcePathFiles = sourcePath.get().asScala.toSeq
        val logicalSourcePath = sourcePathFiles.mkString(File.pathSeparator)
        if sourcePathFiles.nonEmpty then Some(new LogicalPackagesProvider(logicalSourcePath).root) else None
      case SourcePathMode.MBT =>
        Some(ParsedLogicalPackage.fromMbtIndex(semanticdbFileManager.listAllPackages()))
    new CachingDriver(settings, CachedLogicalPackage(sourcePackagesExtractor), compilerAccess, userLogger, emitDiagnostics)

  private val fzaCache = new ConcurrentHashMap[Path, AbstractFile]
  private def fza(path: Path): AbstractFile = {
    val valueOrNull = fzaCache.get(path)
    if (valueOrNull == null) {
      val fza0 = Archives.fileZipArchive(path)
      val previousOrNull = fzaCache.putIfAbsent(path, fza0)
      if (previousOrNull == null) fza0
      else previousOrNull
    }
    else
      valueOrNull
  }
