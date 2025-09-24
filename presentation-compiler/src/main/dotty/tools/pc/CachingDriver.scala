package dotty.tools.pc

import scala.meta.internal.pc.*

import java.net.URI
import java.util as ju

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.config.Platform
import dotty.tools.io.ClassPath
import dotty.tools.dotc.classpath.AggregateClassPath
import java.io.File
import java.nio.file.Paths
import java.nio.file.Path
import java.net.URL
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.classpath.PackageName
import dotty.tools.dotc.classpath.ClassPathEntries
import dotty.tools.dotc.classpath.ClassFileEntry
import dotty.tools.dotc.classpath.SourceFileEntry
import dotty.tools.dotc.classpath.PackageEntry
import dotty.tools.io.FileZipArchive
import java.util.concurrent.ConcurrentHashMap

import scala.compiletime.uninitialized

/**
 * CachingDriver is a wrapper class that provides a compilation cache for InteractiveDriver.
 * CachingDriver skips running compilation if
 * - the target URI of `run` is the same as the previous target URI
 * - the content didn't change since the last compilation.
 *
 * This compilation cache enables Metals to skip compilation and re-use
 * the typed tree under the situation like developers
 * sequentially hover on the symbols in the same file without any changes.
 *
 * Note: we decided to cache only if the target URI is the same as in the previous run
 * because of `InteractiveDriver.currentCtx` that should return the context that
 * refers to the last compiled source file.
 * It would be ideal if we could update currentCtx even when we skip the compilation,
 * but we struggled to do that. See the discussion https://github.com/scalameta/metals/pull/4225#discussion_r941138403
 * To avoid the complexity related to currentCtx,
 * we decided to cache only when the target URI only if the same as the previous run.
 */
class CachingDriver(
    override val settings: List[String],
    javaHome: Path
) extends InteractiveDriver(settings):

  @volatile private var lastCompiledURI: URI = uninitialized

  private var printedCp = false

  override protected def initCtx: Context = {
    val baseCtx: ContextBase = new ContextBase { baseCtx0 =>
      override protected def newPlatform(using Context): Platform = {
        if (baseCtx0.settings.scalajs.value) super.newPlatform
        else
          new dotty.tools.dotc.config.JavaPlatform {
            override def classPath(using Context): ClassPath = {

              val modFiles = baseCtx0.settings.classpath.value
                .split(File.pathSeparator)
                .filter(_.endsWith(".jmod"))
                .map(Paths.get(_))

              lazy val modCp: ClassPath = new ClassPath {
                import java.nio.file._
                import CachingDriver.fza

                lazy val srcZip: Path = {
                  val candidates = List(javaHome.resolve("src.zip"), javaHome.resolve("lib/src.zip"))
                  candidates
                    .iterator
                    .filter(Files.isRegularFile(_))
                    .take(1)
                    .find(_ => true)
                    .getOrElse {
                      sys.error(s"No src.zip found in $candidates")
                    }
                }

                def cpIterator(): Iterator[Path] =
                  modFiles.iterator

                def asClassPathStrings: Seq[String] =
                  cpIterator()
                    .map(_.toString)
                    .toVector
                def asSourcePathString: String =
                  srcZip.toString
                def asURLs: Seq[URL] =
                  cpIterator()
                    .map(_.toUri.toURL)
                    .toVector

                def findClassFile(className: String): Option[AbstractFile] = {
                  val entryName = "classes/" + className.replace(".", "/") + ".class"
                  val idx = entryName.lastIndexOf('/')
                  val dirName = entryName.substring(0, idx + 1)
                  val fileName = entryName.substring(idx + 1)
                  cpIterator()
                    .flatMap { f =>
                      fza(f)
                        .allDirs
                        .get(dirName)
                        .iterator
                        .flatMap(_.entries.get(fileName).iterator)
                    }
                    .take(1)
                    .toList
                    .headOption
                }

                def hasPackage(pkg: PackageName): Boolean = {
                  val dirName = ("classes" +: pkg.dottedString.split('.').filter(_.nonEmpty)).mkString("", "/", "/")
                  cpIterator().exists(f => Option(fza(f).allDirs.get(dirName)).nonEmpty)
                }

                def list(inPackage: PackageName): ClassPathEntries =
                  ClassPathEntries(packages(inPackage), classes(inPackage) ++ sources(inPackage))

                def packages(inPackage: PackageName): Seq[PackageEntry] = {
                  val dirName = ("classes" +: inPackage.dottedString.split('.').filter(_.nonEmpty)).mkString("", "/", "/")
                  val prefix = if (inPackage.dottedString.isEmpty) "" else inPackage.dottedString + "."
                  cpIterator()
                    .flatMap { f =>
                      fza(f)
                        .allDirs
                        .get(dirName)
                        .iterator
                        .flatMap { dirEnt =>
                          dirEnt
                            .entries
                            .valuesIterator
                            .filter(_.isDirectory)
                            .map(e => dotty.tools.dotc.classpath.metals.Entries.packageEntry(prefix + e.name))
                        }
                    }
                    .toVector
                    .distinct
                }
                def classes(inPackage: PackageName): Seq[ClassFileEntry] = {
                  val dirName = ("classes" +: inPackage.dottedString.split('.').filter(_.nonEmpty)).mkString("", "/", "/")
                  cpIterator()
                    .flatMap { f =>
                      fza(f)
                        .allDirs
                        .get(dirName)
                        .iterator
                        .flatMap { dirEnt =>
                          dirEnt
                            .entries
                            .valuesIterator
                            .filter(!_.isDirectory)
                            .filter(_.name.endsWith(".class"))
                            .map(e => dotty.tools.dotc.classpath.metals.Entries.classFileEntry(e))
                        }
                    }
                    .toVector
                    .distinct
                }
                def sources(inPackage: PackageName): Seq[SourceFileEntry] = {
                  val dirName = ("classes" +: inPackage.dottedString.split('.').filter(_.nonEmpty)).mkString("", "/", "/")
                  fza(srcZip)
                    .allDirs
                    .get(dirName)
                    .iterator
                    .flatMap { dirEnt =>
                      dirEnt
                        .entries
                        .valuesIterator
                        .filter(e => e.name.endsWith(".scala") || e.name.endsWith(".java"))
                        .map(e => dotty.tools.dotc.classpath.metals.Entries.sourceFileEntry(e))
                    }
                    .toVector
                    .distinct
                }
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
              val processedCp = process(cp).getOrElse(AggregateClassPath(Nil))
              if (!printedCp) {
                System.err.println("cp = " + pprint.apply(cp))
                System.err.println("processedCp = " + pprint.apply(processedCp))
                printedCp = true
              }
              processedCp match {
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
  }

  private def alreadyCompiled(uri: URI, content: Array[Char]): Boolean =
    compilationUnits.get(uri) match
      case Some(unit)
          if lastCompiledURI == uri &&
            ju.Arrays.equals(unit.source.content(), content) =>
        true
      case _ => false

  override def run(uri: URI, source: SourceFile): List[Diagnostic] =
    val diags =
      if alreadyCompiled(uri, source.content) then Nil
      else super.run(uri, source)
    lastCompiledURI = uri
    diags

  override def run(uri: URI, sourceCode: String): List[Diagnostic] =
    val diags =
      if alreadyCompiled(uri, sourceCode.toCharArray().nn) then Nil
      else super.run(uri, sourceCode)
    lastCompiledURI = uri
    diags

end CachingDriver

object CachingDriver:
  private val fzaCache = new ConcurrentHashMap[Path, FileZipArchive]
  private def fza(path: Path): FileZipArchive = {
    val valueOrNull = fzaCache.get(path)
    if (valueOrNull == null) {
      val fza0 = new FileZipArchive(path, None)
      val previousOrNull = fzaCache.putIfAbsent(path, fza0)
      if (previousOrNull == null) fza0
      else previousOrNull
    }
    else
      valueOrNull
  }
