package better.files

// Dotty specific extensions
import scala.io.BufferedSource

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileReader
import java.io.FileWriter
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.io.RandomAccessFile
import java.nio.channels.AsynchronousFileChannel
import java.nio.channels.FileChannel
import java.nio.file.WatchService
import java.util.zip.ZipFile
import java.util.zip.ZipOutputStream


abstract class HasTill[A] {
  def till(hasMore: => Boolean): Iterator[A]
}

trait Closeable {
  def close(): Unit
}

trait CanClose[T] {
  def close(e: T): Unit
}


object DottyImplicits extends DottyImplicits

trait DottyImplicits {
  implicit object ZOSClose extends CanClose[ZipOutputStream] {
    override def close(e: ZipOutputStream): Unit = e.close()
  }
  implicit object RAFClose extends CanClose[RandomAccessFile] {
    override def close(e: RandomAccessFile): Unit = e.close()
  }
  implicit object BWClose extends CanClose[BufferedWriter] {
    override def close(e: BufferedWriter): Unit = e.close()
  }
  implicit object FRClose extends CanClose[FileReader] {
    override def close(e: FileReader): Unit = e.close()
  }
  implicit object FWClose extends CanClose[FileWriter] {
    override def close(e: FileWriter): Unit = e.close()
  }
  implicit object PWClose extends CanClose[PrintWriter] {
    override def close(e: PrintWriter): Unit = e.close()
  }
  implicit object SClose extends CanClose[Scanner] {
    override def close(e: Scanner): Unit = e.close()
  }
  implicit object OSClose extends CanClose[OutputStream] {
    override def close(e: OutputStream): Unit = e.close()
  }
  implicit object FCClose extends CanClose[FileChannel] {
    override def close(e: FileChannel): Unit = e.close()
  }
  implicit object AFCClose extends CanClose[AsynchronousFileChannel] {
    override def close(e: AsynchronousFileChannel): Unit = e.close()
  }
  implicit object WSClose extends CanClose[WatchService] {
    override def close(e: WatchService): Unit = e.close()
  }
  implicit object ZFClose extends CanClose[ZipFile] {
    override def close(e: ZipFile): Unit = e.close()
  }
  implicit object BRClose extends CanClose[BufferedReader] {
    override def close(e: BufferedReader): Unit = e.close()
  }
  implicit object BSClose extends CanClose[BufferedSource] {
    override def close(e: BufferedSource): Unit = e.close()
  }
  implicit object ISClose extends CanClose[InputStream] {
    override def close(e: InputStream): Unit = e.close()
  }

}
