package dotty.tools.io
package metals

object Archives:
  def fileZipArchive(path: JPath): AbstractFile =
    new FileZipArchive(path, "")
