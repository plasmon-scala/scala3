package dotty.tools.pc

/** What came out of `CachingDriver.reloadSourcePathFile`. */
enum SourcePathReload:
  /** Not a file of this compiler's source path - nothing was done. */
  case NotOnSourcePath
  /** The file holds what the compiler last read from it - nothing was done. */
  case Unchanged
  /** The file was read afresh, and what it defines re-entered. */
  case Reloaded
  /** The file was read afresh, but definitions the compiler used to know from it are gone from it:
    * whatever referred to those keeps them alive, and the compiler had better start over.
    */
  case DefinitionsRemoved
  /** The file is gone. Nothing was done, the compiler had better start over. */
  case Missing
