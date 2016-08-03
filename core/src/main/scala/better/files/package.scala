package better

import scala.collection.mutable
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

package object files extends Implicits {
  type Files = Iterator[File]

  trait Closeable {
    def close(): Unit
  }

  trait CanClose[T] {
    def close(e: T): Unit
  }

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

//  type Closeable = {
//    def close(): Unit
//  }


  type ManagedResource[A] = Traversable[A]

  // Some utils:
  private[files] def newMultiMap[A, B]: mutable.MultiMap[A, B] =
    new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]

  @inline private[files] def when[A](condition: Boolean)(f: => A): Option[A] =
    if (condition) Some(f) else None
  @inline private[files] def repeat(n: Int)(f: => Unit): Unit = (1 to n).foreach(_ => f)

  abstract class HasTill[A] {
    def till(hasMore: => Boolean): Iterator[A]
  }

  private[files] def produce[A](f: => A) = new HasTill[A] {
    def till(hasMore: => Boolean): Iterator[A] = new Iterator[A] {
      override def hasNext = hasMore
      override def next() = f
    }
  }
}
