import java.io.RandomAccessFile
import java.nio._
import java.nio.channels._

val mem = new RandomAccessFile("/home/dahl/trashme", "rw").getChannel().map(FileChannel.MapMode.READ_WRITE, 0, 100);

val zero = 0
val one  = 1

for ( i <- 0 until 100000 ) {
  while ( mem.getInt(4) == zero ) Thread.sleep(0) // waiting for client request
  mem.putInt(8, mem.getInt(2) + 1)                // sending the reply
  mem.putInt(4, zero)
  mem.putInt(0, one)
}

