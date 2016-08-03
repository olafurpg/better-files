package better.files

object Main {
  def main(args: Array[String]) {
    file".".listRelativePaths.foreach(println)
  }

}
