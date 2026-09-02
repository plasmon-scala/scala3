package dotty.tools.pc

import scala.meta.internal.pc.*

import java.net.URI
import java.nio.file.Path
import java.util as ju

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.meta.pc.SemanticdbFileManager
import scala.meta.pc.SourcePathMode

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Denotations.staticRef
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.interactive.{
  CachedLogicalPackage,
  InteractiveDriver,
  LogicalPackage,
  LogicalPackagesProvider,
  ParsedLogicalPackage
}
import dotty.tools.dotc.reporting.Diagnostic
import scala.io.Codec
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

  /** The source path's own file objects, by absolute path.
   *
   *  A file open in the editor is usually on the source path too. The compiler enters symbols for
   *  it from there, with a completer that late-compiles the file on disk - and completing one of
   *  those once the editor's version of the file was entered (a stale companion module, say)
   *  declares everything in the file a second time, as "already defined" errors. `Run.lateCompile`
   *  skips the files of the run's own compilation units, comparing file objects by identity: so the
   *  editor's version gets compiled as this very file object, with the editor's content.
   */
  private lazy val sourcePathFiles: Map[String, AbstractFile] =
    sourcePackage.value(using currentCtx) match
      case None => Map.empty
      case Some(root) =>
        def walk(pkg: LogicalPackage): Iterator[AbstractFile] =
          pkg.sources.iterator ++ pkg.packages.iterator.flatMap(walk)
        walk(root).map(file => (normalizedPath(file.path), file)).toMap

  private def normalizedPath(path: String): String =
    try Paths.get(path).toAbsolutePath.normalize.toString
    catch
      case _: java.nio.file.InvalidPathException => path

  private def asSourcePathFile(source: SourceFile): SourceFile =
    sourcePathFiles.get(normalizedPath(source.file.path)) match
      case Some(file) if !(file eq source.file) =>
        sourceFileWithContent(file, source.content())
      case _ => source

  /** A source file for `file`, but with `fileContent` rather than what is on disk. */
  private def sourceFileWithContent(file: AbstractFile, fileContent: Array[Char]): SourceFile =
    given ctx: Context = currentCtx
    new SourceFile(file, ctx.settings.sourceroot.value, Codec(ctx.settings.encoding.value)):
      override def content(): Array[Char] = fileContent

  /** Reads `path` afresh from disk, when it is on the source path, and re-enters what it defines.
   *
   *  That is what a run on an editor buffer does for the file the buffer holds, and the compiler
   *  takes care of the rest: what an earlier run entered from a file with the same path is unlinked
   *  (`PackageClassDenotation.unlinkFromFile`), same-name entries get replaced, and in interactive
   *  mode a reference to a symbol that went away this way is redirected to its current namesake
   *  (`Denotations.bringForward` via `currentSymbol`).
   *
   *  A definition gone from the file has no namesake to be redirected to, though: whatever referred
   *  to it keeps its former denotation alive, as if the definition were still there. The caller is
   *  told so, for it to start the compiler over instead. Definitions are compared by name (and by
   *  owner), for the top-level classes and objects of the file and, for those the compiler had
   *  completed - the only ones anything can refer to - their members, recursively.
   */
  def reloadSourcePathFile(path: Path): SourcePathReload =
    sourcePathFiles.get(normalizedPath(path.toString)) match
      case None => SourcePathReload.NotOnSourcePath
      case Some(file) =>
        if !file.exists then SourcePathReload.Missing
        else
          val content = new String(file.toByteArray, java.nio.charset.StandardCharsets.UTF_8).toCharArray
          val uri = Paths.get(normalizedPath(file.path)).toUri
          compilationUnits.get(uri) match
            case Some(unit) if ju.Arrays.equals(unit.source.content(), content) =>
              SourcePathReload.Unchanged
            case _ =>
              val before = definitionsFrom(file)(using currentCtx)
              super.run(uri, sourceFileWithContent(file, content))
              // Only what the run just entered counts: a top-level symbol of the file that the
              // run left in place - the file lost its package clause, or moved to another
              // package, or is empty at the moment - is one nothing in the file backs any more
              val (after, survivors) = definitionsFrom(file, currentOnly = true)(using currentCtx)
              val gone = before.diff(after)
              userLogger.accept(
                s"Reloaded $file (${content.length} chars): ${before.size} definitions before, " +
                  s"${after.size} after, ${gone.size} gone, $survivors stale"
              )
              if gone.nonEmpty || survivors > 0 then SourcePathReload.DefinitionsRemoved
              else SourcePathReload.Reloaded

  /** The packages `file` contributes to, per the pre-computed package structure, as dotted names -
   *  the empty package being "".
   */
  private def packagesOf(file: AbstractFile): List[String] =
    val path = normalizedPath(file.path)
    def walk(pkg: LogicalPackage, name: String): List[String] =
      val here =
        if pkg.sources.exists(f => normalizedPath(f.path) == path) then List(name) else Nil
      here ++ pkg.packages.toList.flatMap { sub =>
        walk(sub, if name.isEmpty then sub.name else s"$name.${sub.name}")
      }
    sourcePackage.value(using currentCtx).toList.flatMap(walk(_, ""))

  private def definitionsFrom(file: AbstractFile)(using Context): Set[String] =
    definitionsFrom(file, currentOnly = false)._1

  /** Keys for what `file` defines, as far as the compiler knows: its top-level classes and objects,
   *  and the members of those that are completed, recursively. Nothing gets forced here.
   *
   *  With `currentOnly`, only what the current run entered is keyed, and the top-level symbols of
   *  the file that predate it are counted instead (second element). Checked before anything else
   *  touches those symbols, as touching one brings it forward to the current run.
   */
  private def definitionsFrom(file: AbstractFile, currentOnly: Boolean)(using Context): (Set[String], Int) =
    val path = normalizedPath(file.path)
    val res = scala.collection.mutable.Set.empty[String]
    var survivors = 0
    def members(cls: ClassSymbol, prefix: String): Unit =
      val denot = cls.denot
      if denot.isCompleted then
        for decl <- denot.unforcedDecls.toList if !decl.isConstructor do
          val key = s"$prefix.${decl.name}${if decl.isType then "#" else ""}"
          res += key
          if decl.isClass then members(decl.asClass, key)
    for
      pkgName <- packagesOf(file)
      pkg <- packageClass(pkgName).toList
      sym <- pkg.unforcedDecls.toList
      if sym.isClass
      info = sym.asClass.compUnitInfo
      if info != null && normalizedPath(info.associatedFile.path) == path
    do
      if currentOnly && sym.lastKnownDenotation.validFor.runId != ctx.runId then survivors += 1
      else
        val key = s"$pkgName.${sym.name}"
        res += key
        members(sym.asClass, key)
    (res.toSet, survivors)

  private def packageClass(dottedName: String)(using Context): Option[ClassSymbol] =
    if dottedName.isEmpty then Some(defn.EmptyPackageClass)
    else
      val ref = staticRef(dottedName.toTermName, isPackage = true)
      if ref.exists then Some(ref.symbol.moduleClass.asClass) else None

  private def inSource(source: SourceFile, diagnostic: Diagnostic): Boolean =
    !diagnostic.pos.exists || {
      val file = diagnostic.pos.source.file
      (file eq source.file) || normalizedPath(file.path) == normalizedPath(source.file.path)
    }

  private def alreadyCompiled(uri: URI, content: Array[Char]): Boolean =
    compilationUnits.get(uri) match
      case Some(unit)
          if lastCompiledURI == uri &&
            ju.Arrays.equals(unit.source.content(), content) =>
        true
      case _ => false

  override def run(uri: URI, source0: SourceFile): List[Diagnostic] =
    val source = asSourcePathFile(source0)
    if (!alreadyCompiled(uri, source.content)) {
      val id =
        if (compilerAccess.beforeAccessOpt.nonEmpty || compilerAccess.afterAccessOpt.nonEmpty)
          UUID.randomUUID().toString
        else
          ""
      for (f <- compilerAccess.beforeAccessOpt)
        f(id, "typechecking", uri.toASCIIString)
      try {
        // Only what is located in the file itself: a source path file the run got to
        // late-compile reports its own problems here too - from a genuine error in it, to the
        // "already defined" ones the compiler raises when a generated file re-declares what
        // the edited one already holds (a script and the wrapper the build tool derives from it)
        previousDiags = super.run(uri, source).filter(inSource(source, _))
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
