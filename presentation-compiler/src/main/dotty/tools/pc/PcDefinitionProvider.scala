package dotty.tools.pc

import java.net.URI
import java.nio.file.Paths
import java.util.ArrayList

import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.DefinitionResultImpl
import scala.meta.pc.DefinitionResult
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{Exported, ModuleClass}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.Interactive.Include
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.Location
import java.util.function.Consumer

class PcDefinitionProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    search: SymbolSearch,
    logger: Consumer[String],
    preferSymbolSearch: String => Boolean,
    debug: Boolean
):

  def definitions(module: GlobalSymbolIndex.Module): DefinitionResult =
    definitions(module, findTypeDef = false)

  def typeDefinitions(module: GlobalSymbolIndex.Module): DefinitionResult =
    definitions(module, findTypeDef = true)

  private def definitions(module: GlobalSymbolIndex.Module, findTypeDef: Boolean): DefinitionResult =
    val uri = params.uri().nn
    val text = params.text().nn
    val filePath = Paths.get(uri)
    driver.run(
      uri,
      SourceFile.virtual(filePath.toString, text)
    )

    given ctx: Context = driver.localContext(params)

    val pos = driver.sourcePosition(params)
    val path = MetalsInteractive.workaroundPathIssues {
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
    }

    val indexedContext = IndexedContext(pos)(using ctx)
    val result =
      if findTypeDef then findTypeDefinitions(module, path, pos, indexedContext, uri)
      else findDefinitions(module, path, pos, indexedContext, uri)

    if result.locations().nn.isEmpty() then fallbackToUntyped(module, pos, uri)(using ctx)
    else result
  end definitions

  /** Some nodes might disapear from the typed tree, since they are mostly used
   *  as syntactic sugar. In those cases we check the untyped tree and try to
   *  get the symbol from there, which might actually be there, because these
   *  are the same nodes that go through the typer.
   *
   *  This will happen for:
   *    - `.. derives Show`
   *  @param unit compilation unit of the file
   *  @param pos cursor position
   *  @return definition result
   */
  private def fallbackToUntyped(module: GlobalSymbolIndex.Module, pos: SourcePosition, uri: URI)(
      using ctx: Context
  ) =
    lazy val untpdPath = NavigateAST
      .untypedPath(pos.span)
      .collect { case t: untpd.Tree => t }

    definitionsForSymbols(module, untpdPath.headOption.map(_.symbol).toList, uri, pos)

  private def findDefinitions(
      module: GlobalSymbolIndex.Module,
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      uri: URI
  ): DefinitionResult =
    import indexed.ctx
    val enclosingSymbols = MetalsInteractive.enclosingSymbols(path, pos, indexed)
    if (debug) {
      logger.accept {
        if enclosingSymbols.isEmpty then
          "No enclosing symbols"
        else
          val nl = System.lineSeparator()
          "Enclosing symbols: " + nl + enclosingSymbols.map("  - " + _ + nl).mkString
      }
    }
    definitionsForSymbols(
      module,
      enclosingSymbols,
      uri,
      pos
    )

  private def findTypeDefinitions(
      module: GlobalSymbolIndex.Module,
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      uri: URI
  ): DefinitionResult =
    import indexed.ctx
    val enclosing = path.expandRangeToEnclosingApply(pos)
    val typeSymbols = MetalsInteractive
      .enclosingSymbolsWithExpressionType(enclosing, pos, indexed)
      .map { case (_, tpe, _) =>
        tpe.typeSymbol
      }
    typeSymbols match
      case Nil =>
        path.headOption match
          case Some(value: Literal) =>
            definitionsForSymbols(module, List(value.typeOpt.widen.typeSymbol), uri, pos)
          case _ => DefinitionResultImpl.empty
      case _ =>
        definitionsForSymbols(module, typeSymbols, uri, pos)

  private val nl = System.lineSeparator()
  private def definitionsForSymbols(
      module: GlobalSymbolIndex.Module,
      symbols: List[Symbol],
      uri: URI,
      pos: SourcePosition
  )(using ctx: Context): DefinitionResult =
    val sortedSymbols = PcDefinitionProvider.semanticSymbolsSorted(symbols, identity)
    if (debug)
      logger.accept {
        if (sortedSymbols.isEmpty) "No sorted symbols"
        else
          "Sorted symbols:" + nl +
            sortedSymbols.map(sym => s"  - $sym: ${SemanticdbSymbols.symbolName(sym)}" + nl).mkString
      }
    sortedSymbols match
      case Nil => DefinitionResultImpl.empty
      case syms @ (headSym :: tail) =>
        val locations = syms.map:
          sym =>
            val symName = SemanticdbSymbols.symbolName(sym)
            val locs = locationsForSymbol(module, sym.sourceSymbol, symName, uri, pos)
            if (debug)
              logger.accept {
                if (locs.isEmpty) s"No location found for $symName"
                else
                  s"Locations for $symName:" + nl +
                    locs.map("  " + _ + nl).mkString
              }
            locs
        DefinitionResultImpl(SemanticdbSymbols.symbolName(headSym), locations.flatten.asJava)

  private def locationsForSymbol(
      module: GlobalSymbolIndex.Module,
      symbol: Symbol,
      semanticdbSymbol: String,
      uri: URI,
      pos: SourcePosition
  )(using ctx: Context): List[Location] =
    val isLocal = symbol.source == pos.source
    if isLocal && !preferSymbolSearch(semanticdbSymbol) then
      val trees = driver.openedTrees(uri)
      val include = Include.definitions | Include.local
      val (exportedDefs, otherDefs) =
        Interactive.findTreesMatching(trees, include, symbol)
          .partition(_.tree.symbol.is(Exported))
      otherDefs.headOption.orElse(exportedDefs.headOption).collect:
        case srcTree if srcTree.namePos.exists =>
          new Location(params.uri().toString(), srcTree.namePos.toLsp)
      .toList
    else search.definition(module.asString, semanticdbSymbol, uri).asScala.toList

end PcDefinitionProvider

object PcDefinitionProvider:

  def semanticSymbolsSorted[T](
      syms: List[T],
      f: T => Symbol
  )(using ctx: Context): List[T] =
    syms
      .filter(f(_).exists)
      .map { t =>
        val sym = f(t)
        // in case of having the same type and teerm symbol
        // term comes first
        // used only for ordering symbols that come from `Import`
        val termFlag =
          if sym.is(ModuleClass) then sym.sourceModule.isTerm
          else sym.isTerm
        ((termFlag, SemanticdbSymbols.symbolName(sym)), t)
      }
      .sortBy(_._1)
      .map(_._2)
