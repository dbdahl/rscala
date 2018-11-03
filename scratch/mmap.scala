#!/bin/sh

exec scala "$0" "$@"

!#

import java.io.RandomAccessFile
import java.nio._
import java.nio.channels._
import scala.annotation.tailrec

class Prot(val i: Int, val j: Int) {

  private val mem = new RandomAccessFile("/home/dahl/trashme", "rw").getChannel().map(FileChannel.MapMode.READ_WRITE, 0, 100);

  private val zero = 0
  private val one  = 1

  if ( i == 0 ) mem.putInt(i,one)

  @tailrec
  private def talk(counter: Long, nReps: Long): Unit = {
    while ( mem.getInt(i) == zero ) Thread.sleep(0) // waiting for client request
    mem.putInt(8, mem.getInt(8) + 1)                // sending the reply
    mem.putInt(i, zero)
    mem.putInt(j, one)
    if ( counter < nReps ) talk(counter+1, nReps) else ()
  }

  def talk(nReps: Long): Unit = talk(0, nReps)

}


val prot = new Prot(args(0).toInt, args(1).toInt)
prot.talk(100000000L)

