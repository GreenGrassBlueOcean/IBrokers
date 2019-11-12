serverVersion <- function(con) {
  if(!is.twsConnection(con))
    stop('con must be a twsConnection object')

  con$server.version
}



CheckServerVersion <- function(conn, requiredVersion )
{
  serverVersion <- serverVersion(conn)
  
  if (serverVersion < requiredVersion)
  { print("Required server version is too low")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

