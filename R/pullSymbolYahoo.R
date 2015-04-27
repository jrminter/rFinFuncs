#' Pull financial data from Yahoo! Finance
#'
#' A helper function to pull data. Inspired by quantmod
#'
#' @param Symbol The stock symbol, e.g. \code{"^VIX"}
#'
#' @param from A start date (default "2009-01-01")
#'
#' @param to An ending date (default "" is Sys.Date())
#'
#' @param savePath (default "") A path ("~/dat") to optionally save csv
#'
#' @param bVerbose A boolean flag (default FALSE) to print info
#'
#' @return A time series zoo object
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' library(rFinFuncs)
pullSymbolYahoo <- function(Symbol,
                            from="2009-01-01",
                            to="",
                            savePath="",
                            bVerbose=FALSE){
  if (nchar(to) == 0){
    to <- Sys.Date()
  }
  tmp <- "test.csv"
  yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?"
  from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
  from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
  from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
  to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
  to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
  to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
  strFile <- paste(yahoo.URL,
                  "s=",Symbol,
                  "&a=",from.m,
                  "&b=",sprintf('%.2d',from.d),
                  "&c=",from.y,
                  "&d=",to.m,
                  "&e=",sprintf('%.2d',to.d),
                  "&f=",to.y,
                  "&g=d&q=q&y=0",
                  "&z=",Symbol,"&x=.csv",
                   sep='')
  if(bVerbose==TRUE){
    print(strFile)
  }

  download.file(strFile, destfile=tmp)
  if(nchar(savePath) > 0){
    strSave <- paste0(savePath,'/', Symbol, '.csv')
    # save a copy
    file.copy(tmp, strSave)
  }
  fr <- read.csv(tmp)
  fr <- xts(as.matrix(fr[,-1]),
            as.Date(fr[,1]),
            src='yahoo',updated=Sys.time())

  colnames(fr) <- paste(toupper(gsub('\\^','',Symbol)),
                        c('Open','High','Low','Close',
                          'Volume','Adjusted'),
                        sep='.')
  unlink(tmp)
  if(bVerbose==TRUE){
    print(head(fr))
    print(tail(fr))
    print(class(fr))
  }
  return(fr)
}
