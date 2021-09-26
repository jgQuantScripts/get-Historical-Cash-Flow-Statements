require("httr");require("rvest");require("xml2");require("quantmod")

# ticker to get CF
ticker = "AAPL"

# this function will get you the latest quarterly CF (5 quarters)
getLatestCF = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/cash-flow/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  
  # get rid of special characters
  df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
  df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
  df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
  df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
  df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
  
  # add VAL to each numeric variable in CF statement otherwise NA
  for(ii in 2:ncol(df)){
  df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                     df[,ii], 
                                     as.numeric(paste0(df[,ii],VAL))
                                     )
                              )
             )
  }
  # remove ALL empty rows
  df = df[!(rowSums(is.na(df)) == 5),]
  # change duplicate row name
  df$Description[nrow(df)-2] <- "Operating CF"
  # make sure it is data.frame
  df <- data.frame(df[,2:6],row.names = df$Description)
  # return data.frame
  df
}
# test function
pg1 = getLatestCF(ticker=ticker)

# this function will get you 9 pages of quarterly CF (45 quarters)
getRestCF = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/cash-flow/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(pg, length)>0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    
    # get rid of special characters
    df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
    df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
    df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
    df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
    df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
    
    # add VAL to each numeric variable in CF statement otherwise NA
    for(ii in 2:ncol(df)){
      df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                    df[,ii], 
                                                    as.numeric(paste0(df[,ii],VAL))
      )
      )
      )
    }
    # remove ALL empty rows
    df = df[!(rowSums(is.na(df)) == 5),]
    # change duplicate row name
    df$Description[nrow(df)-2] <- "Operating CF"
    # make sure it is data.frame
    df <- data.frame(df[,2:6],row.names = df$Description)
    # return data.frame
    df
  })
  # merge by row.names
  df2 = merge(df[[1]],df[[2]],by="row.names", all=TRUE)
  df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  # merge by row names and convert to data.frame
  for(ii in 3:length(df)){
    df2 = merge(df2,df[[ii]],by="row.names", all=TRUE) %>% suppressWarnings()
    df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  }
  # return df2
  df2
}
# test function
PGS = getRestCF(ticker=ticker)
# combine tables & merge by row.names
CF = merge(pg1,PGS,by="row.names", all=TRUE)
CF = data.frame(CF[,2:ncol(CF)], row.names = CF[,1])


# add CF ratios
getCFRatios = function(CF,ticker)
{
  # https://www.oldschoolvalue.com/stock-valuation/cash-flow-ratios/
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert CF - names to Quarterly timestamps
  Qdates = as.yearqtr(names(CF)[1:length(CF)], format="X%m.%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 1:ncol(CF))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(CF)[ii], format="X%m.%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # data frame// format rows
  toR <- data.frame(toR[,2:ncol(toR)], row.names = "Stock Price")
  colnames(toR) <- names(CF)
  # ***************************************************************************************
  #  Operating Cash Flow / Total Cash Flow: How much of the cash comes from Operations
  # ***************************************************************************************
  op2TotalCF = round(as.numeric(CF["Operating Cash Flow",])/
    (as.numeric(CF["Operating Cash Flow",])+
     as.numeric(CF["Financing Cash Flow",])+
     as.numeric(CF["Investing Cash Flow",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  op2TotalCF <- data.frame(op2TotalCF, row.names = "Cash Generating Ratio")
  colnames(op2TotalCF) <- names(CF)
  # ***************************************************************************************
  #  External Financing Ratio (CFF / CFO): How much does the company depend on Financing
  # ***************************************************************************************
  EFR = round(as.numeric(CF["Financing Cash Flow",])/
                       as.numeric(CF["Operating Cash Flow",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  EFR <- data.frame(EFR, row.names = "External Financing Ratio")
  colnames(EFR) <- names(CF)
  
  
  
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(toR,op2TotalCF,EFR)
  
  ALL
}
# test function
RATIOS = getCFRatios(CF=CF,ticker=ticker)
# rowbind with CF
CF = rbind(CF,RATIOS)

# write table as csv
write.table(CF,paste0("~/Desktop/",ticker,"_CF.csv"),sep = ",")


# transpose table
CFt = as.data.frame(t(CF))
# write table as csv
write.table(CFt,paste0("~/Desktop/",ticker,"_CFT.csv"),sep = ",")

