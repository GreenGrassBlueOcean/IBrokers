.reqContractDetails <- function(conn, Contract, reqId="1")
{
    if(!is.twsConnection(conn))
      stop("requires twsConnection object")

    if(!inherits(Contract, "twsContract"))
      stop("requires twsContract object")

    con <- conn[[1]]

    VERSION <- "5"
    VERSION <- "6"
    VERSION <- "7"

    request <- c(.twsOutgoingMSG$REQ_CONTRACT_DATA,
                 VERSION,
                 reqId,
                 Contract$conId,
                 Contract$symbol,
                 Contract$sectype,
                 Contract$expiry,
                 Contract$strike,
                 Contract$right,
                 Contract$multiplier,
                 Contract$exch,
                 Contract$currency,
                 Contract$local,
                 "",  # Contract$tradingClass,  # not using
                 Contract$include_expired,
                 Contract$secIdType,
                 Contract$secId)
    writeBin(as.character(request), con)            
}

reqContractDetails <-
function(conn, Contract, reqId="1", verbose=FALSE,
         eventWrapper=eWrapper(), CALLBACK=twsCALLBACK, ...) {

    .reqContractDetails(conn, Contract, reqId)

    if(is.null(CALLBACK))
      invisible(return(NULL))

    # create an internal eWrapper to *only* handle contract request data,
    # all else is discarded.
    eW <- eWrapper(NULL)
    eW$contractDetails <- function(curMsg, msg, timestamp, file, ...) {
      # custom contractData function called from processMsg
      twsContractDetails(version=msg[1],
                         #reqId=msg[2],
                         contract=twsContract(conId=msg[12+1],
                                              symbol=msg[3],
                                              sectype=msg[4],
                                              expiry=msg[5],  
                                              primary=msg[21],
                                              strike=msg[5+1],
                                              right=msg[6+1],
                                              exch=msg[7+1],
                                              currency=msg[8+1],
                                              multiplier=msg[14+1],
                                              include_expired=Contract$include_expired,
                                              combo_legs_desc="", comboleg="",
                                              local=msg[9+1]),
                         marketName=msg[10+1],
                         tradingClass=msg[11+1],
                         conId=msg[12+1],
                         minTick=msg[13+1],
                         orderTypes=unlist(strsplit(msg[15+1],",")),
                         validExchanges=unlist(strsplit(msg[16+1],",")),
                         priceMagnifier=msg[17+1],
                         underConId=msg[18+1],
                         longName=msg[19+1],
                         contractMonth=msg[22],
                         industry=msg[23],
                         category=msg[24],
                         subcategory=msg[25],
                         timeZoneId=msg[26],
                         tradingHours=msg[27],
                         liquidHours=msg[28])
    }

    contracts <- list()
    con <- conn[[1]]
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL) 
      curMsg <- readBin(con, character(), 1)
      if(curMsg != .twsIncomingMSG$CONTRACT_DATA) {
        if(curMsg == .twsIncomingMSG$ERR_MSG) {
          if(!errorHandler(con,verbose,OK=c(165,300,366,2104,2106,2107))){
            warning("error in contract details")
            break
          }
        } else {
          processMsg(curMsg, con, eW, timestamp, file)
          if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END)
            break      
        }
      }
      if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
      contracts[[length(contracts)+1]] <-
        processMsg(curMsg, con, eW, timestamp, file)
      }

    }
    return(contracts)
}



# 
# /**
#   * @brief Requests contract information.\n
# * This method will provide all the contracts matching the contract provided. It can also be used to retrieve complete options and futures chains. This information will be returned at EWrapper:contractDetails. Though it is now (in API version > 9.72.12) advised to use reqSecDefOptParams for that purpose. \n
# * @param reqId the unique request identifier.\n
# * @param contract the contract used as sample to query the available contracts. Typically, it will contain the Contract::Symbol, Contract::Currency, Contract::SecType, Contract::Exchange\n
# * @sa EWrapper::contractDetails, EWrapper::contractDetailsEnd
# */
#   public void reqContractDetails(int reqId, Contract contract)
# {
#   if (!CheckConnection())
#     return;
#   
#   if (!IsEmpty(contract.SecIdType) || !IsEmpty(contract.SecId))
#   {
#     if (!CheckServerVersion(reqId, MinServerVer.SEC_ID_TYPE, " It does not support secIdType not secId attributes"))
#       return;
#   }
#   
#   if (!IsEmpty(contract.TradingClass))
#   {
#     if (!CheckServerVersion(reqId, MinServerVer.TRADING_CLASS, " It does not support the TradingClass parameter when requesting contract details."))
#       return;
#   }
#   
#   if (!IsEmpty(contract.PrimaryExch) && !CheckServerVersion(reqId, MinServerVer.LINKING,
#                                                             " It does not support PrimaryExch parameter when requesting contract details."))
#     return;
#   
#   
#   int VERSION = 8;
#   
#   var paramsList = new BinaryWriter(new MemoryStream());
#   var lengthPos = prepareBuffer(paramsList);
#   
#   paramsList.AddParameter(OutgoingMessages.RequestContractData);
#   paramsList.AddParameter(VERSION);//version
#   if (serverVersion >= MinServerVer.CONTRACT_DATA_CHAIN)
#   {
#     paramsList.AddParameter(reqId);
#   }
#   if (serverVersion >= MinServerVer.CONTRACT_CONID)
#   {
#     paramsList.AddParameter(contract.ConId);
#   }
#   paramsList.AddParameter(contract.Symbol);
#   paramsList.AddParameter(contract.SecType);
#   paramsList.AddParameter(contract.LastTradeDateOrContractMonth);
#   paramsList.AddParameter(contract.Strike);
#   paramsList.AddParameter(contract.Right);
#   if (serverVersion >= 15)
#   {
#     paramsList.AddParameter(contract.Multiplier);
#   }
#   
#   if (serverVersion >= MinServerVer.PRIMARYEXCH)
#   {
#     paramsList.AddParameter(contract.Exchange);
#     paramsList.AddParameter(contract.PrimaryExch);
#   }
#   else if (serverVersion >= MinServerVer.LINKING)
#   {
#     if (!IsEmpty(contract.PrimaryExch) && (contract.Exchange == "BEST" || contract.Exchange == "SMART"))
#     {
#       paramsList.AddParameter(contract.Exchange + ":" + contract.PrimaryExch);
#     }
#     else
#     {
#       paramsList.AddParameter(contract.Exchange);
#     }
#   }
#   
#   paramsList.AddParameter(contract.Currency);
#   paramsList.AddParameter(contract.LocalSymbol);
#   if (serverVersion >= MinServerVer.TRADING_CLASS)
#   {
#     paramsList.AddParameter(contract.TradingClass);
#   }
#   if (serverVersion >= 31)
#   {
#     paramsList.AddParameter(contract.IncludeExpired);
#   }
#   if (serverVersion >= MinServerVer.SEC_ID_TYPE)
#   {
#     paramsList.AddParameter(contract.SecIdType);
#     paramsList.AddParameter(contract.SecId);
#   }
#   CloseAndSend(reqId, paramsList, lengthPos, EClientErrors.FAIL_SEND_REQCONTRACT);
#   }
