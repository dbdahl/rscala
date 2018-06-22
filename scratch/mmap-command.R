library(mmap)
library(ff)

itFilename  <- "/home/dahl/mmap-it.bin"
commandFilename  <- "/home/dahl/mmap-cmd.bin"
lengthFilename   <- "/home/dahl/mmap-length.bin"
dataFilename     <- "/home/dahl/mmap-data.bin"

#writeBin(raw(2), itFilename, endian="big")
#writeBin(raw(2), commandFilename, endian="big")
#writeBin(integer(1), lengthFilename, endian="big")
#writeBin(double(100), dataFilename, endian="big")

mIt       <- ff(filename=itFilename, length=2, vmode="byte")
mCommand  <- ff(filename=commandFilename, length=2, vmode="byte")
mLength   <- ff(filename=lengthFilename, length=1000, vmode="integer")
mInt      <- ff(filename=dataFilename, length=100, vmode="integer")
mDouble   <- ff(filename=dataFilename, length=100, vmode="double")

#mIt       <- mmap(file=itFilename, mode=char())
#mCommand  <- mmap(file=commandFilename, mode=char())
#mLength   <- mmap(file=lengthFilename, mode=integer())
#mInt      <- mmap(file=dataFilename, mode=integer())
#mDouble   <- mmap(file=dataFilename, mode=double())

CMD_IT_SERVER <- as.raw(1)
CMD_IT_CLIENT <- as.raw(2)
CMD_PUSH <- as.raw(10)
CMD_POP  <- as.raw(11)
CMD_ECHO <- as.raw(12)

waitFor <- function(IT) {
  while ( mIt[1] != IT ) {}
}

releaseTo <- function(IT) {
  mIt[1] <- IT
}

