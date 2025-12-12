package dotty.tools.pc

import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.ItemResolver
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.CompletionItem

object CompletionItemResolver extends ItemResolver:

  override def adjustIndexOfJavaParams = 0

  def resolve(
      module: GlobalSymbolIndex.Module,
      item: CompletionItem,
      msym: String,
      search: SymbolSearch,
      metalsConfig: PresentationCompilerConfig
  )(using Context): CompletionItem =
    SemanticdbSymbols.inverseSemanticdbSymbol(msym) match
      case gsym :: _ if gsym != NoSymbol =>
        search
          .symbolDocumentation(module, gsym)
          .orElse(
            search.symbolDocumentation(module, gsym.companion)
          ) match
          case Some(info) if item.getDetail() != null =>
            enrichDocs(
              item,
              info,
              metalsConfig,
              fullDocstring(module, gsym, search),
              gsym.is(JavaDefined)
            )
          case _ =>
            item
        end match

      case _ => item
    end match
  end resolve

  private def fullDocstring(module: GlobalSymbolIndex.Module, gsym: Symbol, search: SymbolSearch)(using
      Context
  ): String =
    def docs(gsym: Symbol): String =
      search.symbolDocumentation(module, gsym).fold("")(_.docstring().nn)
    val gsymDoc = docs(gsym)
    def keyword(gsym: Symbol): String =
      if gsym.isClass then "class"
      else if gsym.is(Trait) then "trait"
      else if gsym.isAllOf(JavaInterface) then "interface"
      else if gsym.is(Module) then "object"
      else ""
    val companion = gsym.companion
    if companion == NoSymbol || gsym.is(JavaDefined) then
      if gsymDoc.isEmpty() then
        if gsym.isAliasType then
          fullDocstring(module, gsym.info.deepDealiasAndSimplify.typeSymbol, search)
        else if gsym.is(Method) then
          gsym.info.finalResultType match
            case tr @ TermRef(_, sym) =>
              fullDocstring(module, tr.symbol, search)
            case _ =>
              ""
        else if gsym.isTerm && gsym.info.typeSymbol.is(Module) then
          fullDocstring(module, gsym.info.typeSymbol.companion, search)
        else ""
      else gsymDoc
    else
      val companionDoc = docs(companion)
      if companionDoc.isEmpty() || companionDoc == gsymDoc then gsymDoc
      else if gsymDoc.isEmpty() then companionDoc
      else
        List(
          s"""|### ${keyword(companion)} ${companion.name}
              |$companionDoc
              |""".stripMargin,
          s"""|### ${keyword(gsym)} ${gsym.name}
              |${gsymDoc}
              |""".stripMargin
        ).sorted.mkString("\n")
    end if
  end fullDocstring

end CompletionItemResolver
