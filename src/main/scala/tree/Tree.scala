package tree

/**
 * Hello world!
 *
 */
object Tree extends Application {
  private var dirNum = 0;
  private var fileNum = 0;

  private def tree(dir: java.io.File, branch: String): List[String] = {
    var curStr:List[String] = Nil
    val files = dir.listFiles.toList.
                filter(!_.getName.startsWith(".")).reverse

    for (i <- 0 until files.length) {
      val name = files(i).getName
      val curBranch1 = if (i == files.length - 1) "    "
                       else "|   "
      val curBranch2 = if (i == files.length - 1) "`-- "
                       else "|-- "

      if (files(i).isDirectory) {
        dirNum += 1
        curStr = tree(files(i), branch + curBranch1) :::
                  ((branch + curBranch2 + name) :: curStr)
      }
      else {
        fileNum += 1
        curStr = (branch + curBranch2 + name) :: curStr
      }
    }

    curStr
  }

  override def main(args: Array[String]) {
    val treeList = tree(new java.io.File(args(0)), "") ::: List(".")

    println(treeList.reverse.mkString("\n") +
            "\n\n%d directories, %d files".format(dirNum, fileNum))
  }
}
