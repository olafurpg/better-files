package better.files

object Main {
  def main(args: Array[String]): Unit = {
    file".".listRelativePaths.foreach(println)
  }
}
