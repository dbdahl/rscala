source("mmap-command.R")

# Server

while ( TRUE ) {
  waitFor(CMD_IT_SERVER)
  cmd <- mCommand[1]
  if ( cmd == CMD_PUSH ) {
    d <- mInt[1]
    releaseTo(CMD_IT_CLIENT)
  } else if ( cmd == CMD_POP ) {
    mDouble[1] <- d
    releaseTo(CMD_IT_CLIENT)
  } else stop("Unknown command.")
}

