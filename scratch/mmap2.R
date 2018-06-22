library(mmap)

commandFilename  <- "/home/dahl/mmap-command.bin"
lengthFilename   <- "/home/dahl/mmap-length.bin"
dataFilename     <- "/home/dahl/mmap-data.bin"

writeBin(raw(1), commandFilename, endian="big")
writeBin(integer(1), lengthFilename, endian="big")
writeBin(double(100), dataFilename, endian="big")

mCommand  <- mmap(file=commandFilename, mode=char())
mLength   <- mmap(file=lengthFilename, mode=integer())
mInt      <- mmap(file=dataFilename, mode=integer())
mDouble   <- mmap(file=dataFilename, mode=double())

CMD_PUT <- as.raw(1)

mCommand[1] <- c(CMD_PUT,

