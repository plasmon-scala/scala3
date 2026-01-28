package dotty.tools.pc

import java.util.{List as JList}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.pc.CompileResult
import scala.meta.pc.VirtualFileParams
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts.Context

object CompileProvider:

  def compile(
      params: VirtualFileParams,
      driver: InteractiveDriver
  )(implicit reportContext: ReportContext): CompileProvider.Result =
    val uri = params.uri().nn
    val sourceFile = SourceFile.virtual(uri, params.text().nn)
    driver.run(uri, sourceFile)
    val unit = driver.compilationUnits.get(uri)

    given ctx: Context =
      val ctx = driver.currentCtx
      unit.map(ctx.fresh.setCompilationUnit).getOrElse(ctx)

    val code = unit
      .map(u => List(u.tpdTree))
      .getOrElse(driver.openedTrees(uri).map(_.tree))
      .map(_.showIndented(2))
      .mkString("\n- ")

    val diag = driver.currentCtx.reporter.allErrors ++
      driver.currentCtx.reporter.allWarnings ++
      driver.currentCtx.reporter.allInfos

    CompileProvider.Result(
      Nil,
      code + "\n\n\n/*\n\n" + diag.map(_.toString + "\n").mkString + "\n*/"
    )
  end compile

  final case class Result(
      scalaDiagnostics: List[String],
      fullTree: String
  ) extends CompileResult {
    def diagnostics(): JList[String] = scalaDiagnostics.asJava
  }

end CompileProvider
