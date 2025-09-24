package dotty.tools.dotc.classpath
package metals

import dotty.tools.io.AbstractFile

object Entries:
  def packageEntry(name: String): PackageEntry =
    PackageEntryImpl(name)
  def classFileEntry(file: AbstractFile): ClassFileEntry =
    ClassFileEntry(file)
  def sourceFileEntry(file: AbstractFile): SourceFileEntry =
    SourceFileEntry(file)