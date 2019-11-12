


cancelFundamentalData <- function(twsconn, reqId){
  # * @brief Cancels Fundamental data request
  #  @param reqId the request's identifier.
  
            if (!isConnected(twsconn))
              {
                print("no TWS Connection!")
                stop()
              }
            if (!CheckServerVersion(twsconn, .twsMinServerVer$FUNDAMENTAL_DATA))
              {
                 print("Server version too low for fundamental data requests.")
                 stop()
               }  
                
            SendCancelRequest(twsconn, .twsOutgoingMSG$CANCEL_FUNDAMENTAL_DATA, 1, reqId, .EClientErrors$FAIL_SEND_CANFUNDDATA);
        }




SendCancelRequest <- function(twsconn, OutgoingMSG, VERSION, reqId, EClientError) {
  print("hello")
  tryCatch({
    msg <- c( OutgoingMSG, VERSION,reqId)
    writeBin( as.character(msg), twsconn[[1]])

  }, error = function(e) {print(paste(EClientError, collapse = ", ")) }
  )

}

errorHandler <- function(con, verbose, OK=NULL) {
  err <- readBin(con,character(),4)
  
  if(as.numeric(err[3]) %in% OK || as.numeric(err[3]) > 1000) {
    if(as.numeric(err[3]) == 1100) {
      
      #close(con)  # instead of closing connection, we'll 
      warning(err[4])
    }
    if(verbose > 1) {
      warning(err[4])
      return(TRUE)
    } else return(TRUE)
  } else {
    if(verbose > 0) #warning(paste(.twsERR[err[3],]))
      warning(err[4])
    return(FALSE)
  }
}




reqFundamentalData <- function(twsconn, reqId, contract, reportType) {
  if( !is.twsConnection(twsconn))
    stop('invalid twsConnection')
  if( !is.twsContract(contract))
    stop('invalid twsContract')
  if( !CheckServerVersion(conn = twsconn, requiredVersion = .twsMinServerVer$FUNDAMENTAL_DATA))
    stop('this ServerVersion does not support Fundamental Data requests')
  
  

  VERSION <- "3"

  msg <- c( .twsOutgoingMSG$REQ_FUNDAMENTAL_DATA,
            VERSION,
            reqId,
            
            # contract fields
            contract$symbol,
            contract$sectype,
            contract$exch,
            contract$primary,
            contract$currency,
            contract$local,

            reportType)

  
  CloseAndSend(reqId, msg, twsconn, EClientErrors.FAIL_SEND_REQFUNDDATA);
  
  # writeBin( as.character(msg), twsconn[[1]])
}


CloseAndSend <- function(reqId, msg, twsconn, ErrorMessage, verbose = TRUE){
  
  writeBin( as.character(msg), twsconn[[1]])
  
  waiting <- TRUE           # waiting for valid response?
  response <- character(0)  # currently read response
  
  if(verbose) {
    cat('waiting for TWS reply on',Contract$symbol,'...')
    iter <- 1
    flush.console()
  }
  
  while(waiting) {
    if( !socketSelect(list(twsconn), FALSE, 0.25))
      next
    curMsg <- readBin(twsconn,character(),1)
    if(verbose) {
      cat('.')
      if(iter %% 30 == 0) cat('\n')
      flush.console()
      iter <- iter + 1
      #Sys.sleep(0.25)
    }
    
    if(length(curMsg) > 0) {
      # watch for error messages
      if(curMsg == .twsIncomingMSG$ERR_MSG) {
        if(!errorHandler(twsconn,verbose,OK=c(165,300,366,2104,2106,2107))) {
          cat('failed.\n')
          #stop('Unable to complete historical data request', call.=FALSE)
          on.exit()
          invisible(return())
        }
      }
      else {return(curMsg)}
    }
      
  }  
}   
      
#       
#       
#       # watch for historical data start
#       if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
#         header <- readBin(twsconn,character(),5)
#         nbin <- as.numeric(header[5])*9
#         req.from <- header[3]
#         req.to   <- header[4]
#         Sys.sleep(2) # add delay for Windows issues - readBin on M$ is bad, bad, bad...
#         response <- readBin(twsconn,character(),nbin)
#         waiting <- FALSE
#         if(verbose) {
#           cat(' done.\n')
#           flush.console()
#         }
#         on.exit()
#       }
#     }
#   }
#   
#   if(missing(eventHistoricalData)) {
#     # the default: return an xts object
#     cm <- matrix(response,ncol=9,byrow=TRUE)
#     cm[,8] <- ifelse(cm[,8]=='false',0,1)
#     if(timeFormat==2 && !nchar(cm[1,1]) > 8) {  # IB ignores the timeFormat if daily returns
#       dts <- structure(as.numeric(cm[,1]), class=c("POSIXct","POSIXt"), tzone=tzone)
#     } else {
#       dts <- structure(as.numeric(as.POSIXlt(gsub('(\\d{4})(\\d{2})(\\d{2})','\\1-\\2-\\3',cm[,1],perl=TRUE))),
#                        class=c("POSIXct","POSIXt"), tzone=tzone)
#     }
#     
#     # if file is specified - dump to file instead
#     if(!missing(file)) {
#       cm[,1] <- dts
#       write.table(cm,
#                   file=file,
#                   quote=FALSE,
#                   row.names=FALSE,
#                   col.names=FALSE,
#                   sep=',')
#       invisible(return())
#     }
#     
#     #x <- xts(matrix(as.numeric(cm[,-1]),nc=8),order.by=structure(as.numeric(as.POSIXlt(dts)), class=c("POSIXt", "POSIXct")))
#     x <- xts(matrix(as.numeric(cm[,-1]),ncol=8),order.by=dts, tzone=tzone)
#     localsymbol <- reqContractDetails(conn, Contract)[[1]]$contract$local
#     colnames(x) <- paste(localsymbol, c('Open','High','Low','Close','Volume',
#                                         'WAP','hasGaps','Count'), sep='.')
#     xtsAttributes(x) <- list(from=req.from,to=req.to,
#                              src='IB',updated=Sys.time())
#     return(x)
#   } else
#     if(is.null(eventHistoricalData)) {
#       # return raw TWS data including header
#       return(c(header,response))
#     } else {
#       # pass to callback function
#       FUN <- match.fun(eventHistoricalData)
#       return(FUN(c(header,response)))
#     }
#   
# }  
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# cancelFundamentalData <- function(twsconn, reqId) {
#   if( !is.twsConnection(twsconn))
#     stop('invalid twsConnection')
# 
#   VERSION <- "1"
# 
#   msg <- c( .twsOutgoingMSG$CANCEL_FUNDAMENTAL_DATA,
#             VERSION,
#             reqId)
# 
#   writeBin( as.character(msg), twsconn[[1]])
# }


