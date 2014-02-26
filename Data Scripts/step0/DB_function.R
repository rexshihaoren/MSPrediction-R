## DB functions

### GENERAL SIMPLE STUFF

find_field <- function (object, s_field, ignore.case = T, ...) {
  ret <- FALSE
  if (is.list(object)) {
    NAMES <- names(object)
    i_NAMES <- grep(s_field,NAMES,ignore.case,...)
    if(any(i_NAMES)) {
#       cat(paste(NAMES[i_NAMES] , '\n', collapse = '') )
#       utils:::str.default(object[i_NAMES], max.level=1)
      cat('----- \n')
      cat(paste(capture.output(str(object[i_NAMES], max.level=1))[-1], '\n', collapse =''))
      cat('----- \n')
      ret <- TRUE }
    for (s_name in NAMES) {
      if (find_field (object[[s_name]], s_field, ...)) {
        cat(paste0('\t ... FROM :', s_name, '\n'))
        cat('------------------- \n')
        ret <- TRUE }
    }
  }
  return (ret)
}

removeFields <- function (df, patterns, DEBUG = F, ...) {
  i_fields <- unlist(lapply(patterns, grep, colnames(df), ...))
  df <- df[-i_fields]
  message('DEBUG: Removing fields:\n', paste(colnames(df)[i_fields], collapse = '; '))
}

### TABLE ANALYSIS

quick_analysis_table <- function (table_i, s_table = deparse(substitute(table_i)) ) {
  table_i2 <- data.frame(lapply(table_i, as.vector), stringsAsFactors=TRUE)
#   png(paste('./DB_images/quick_analysis_',s_table,'%03d.pdf',sep="") )
#   par(ask = TRUE)
  num <- 0
  for (s_col in colnames(table_i2) ){
    num <- num + 1
    cat(paste("treating column", s_col,"\n"))
    plot_col(table_i2[s_col], s_table, num)
  }
  cat("DONE\n")
  
}
  
require(ggplot2)

plot_col <- function (dfcol, s_table='', num=0){
  ow <- options(warn = 1) # show warning at runtime
  data = dfcol[[1]]
  s_data = colnames(dfcol)
  
  if (all(is.na(data))){ # Null column
    plotText('NULL', s_data, s_table, num)
  }
  
  else if ( sum(!is.na(data)) < length(data)/10 ){ # Column with too few values
    plotText('DISCARDED', s_data)
    addplotNAData(data, type = 'big')
  }
  
  else if (length(grep("date", s_data, ignore.case = TRUE)) & is.factor(data)) { # Let's go for some date parsing !
    tryCatch({
      date_data <- as.Date(data)
      if (mean(year(date_data), na.rm = T) < 1950) #Wrong parsing
        error('Wrong parsing')  
      breakspec <- ifelse(diff(range(year(date_data), na.rm = T))>8, 'years', 'quarters')
      hist(date_data, breaks = breakspec, freq =T, ylab = "Counts", xlab = breakspec, main=paste("'",s_data,"' from ", s_table, "(col", num, ")"))
      addplotNAData(data) 
    },
             error = eHandler <- function(err) {
               tryCatch({
                 date_data <- parse_date(as.character(data))
                 breakspec <- ifelse(diff(range(year(date_data), na.rm = T))>8, 'years', 'quarters')
                 hist(date_data, breaks = breakspec, freq =T, ylab = "Counts", xlab = breakspec, main=paste("'",s_data,"' from ", s_table))
                 addplotNAData(data, third = date_data) 
               },
                        error = eHandler2 <- function(err) {
                          plotText('WEIRD DATE', s_data)
                          addplotNAData(data, type='big')
                          print(paste('debug=error',err))
                        })
             })
  }
  
  else if (is.numeric(data)){
    outliers <- error_detect(data)
    if (length(unique(outliers$data)) < 3)
      plotFactor(factor( c( as.character(outliers$data), rep(suppressWarnings(paste(min(outliers$points),max(outliers$points),sep='->')), length(outliers$points)) ) ), s_data)
    else {
#       colors = cm.colors()
      hist(outliers$data, breaks = 50, ylab = 'counts', xlab = s_data, main=paste("'",s_data,"' from ", s_table, "(col", num, ")"))
      addplotSummary(dfcol, outliers = outliers$bool)
      addplotNAData(data)
    }
  }
  
  else if (is.factor(data)){
    plotFactor(data, s_data, s_table, num)
    addplotSummary(dfcol) 
  }
  options(ow)
#   print(warnings())  NO BECAUSE IT KEEPS THEM! and display each time without flushing... 
}

## SUB FUNCTIONS

error_detect <- function (x, factor = 5) {
  qnt <- quantile(x, c(0.25,0.75), na.rm = TRUE)
  IQT <- qnt[2] - qnt[1]
  index_bool <- x > qnt[2] + factor*IQT | x < qnt[1] - factor*IQT  # FAT selction rule !
  points <- x[index_bool & !is.na(index_bool) ]
  data <- x[!index_bool]
  bool <- any(index_bool[!is.na(index_bool)])
  return(list(index_bool = index_bool, points = points, data = data, bool = bool))
}

plotText <- function(s_text, s_data, s_table='', num=0) {
  plot.default(c(0,0,1,1), c(0,1,0,1), main=paste("'",s_data,"' from ", s_table, "(col", num, ")"), xlab = '', ylab = '', xaxt='n', yaxt='n')
  text(0.5,0.5, s_text, cex = 4)
}

plotFactor <- function(data, s_data, s_table='', num=0) {
  if (length(levels(data)) > 8) {
    plotText('labels', s_data)
    addplotNAData(data, type='big')
    addplotSummary(data.frame(data))
  }
  else {
    xbar <- plot(addNA(data), main=paste("'",s_data,"' from ", s_table, "(col", num, ")"), names.arg=c(levels(data),"NA") )
    heights <- table(addNA(data))
    maxh <- max(heights)
    if (length(labels <- xbar[heights < maxh*0.8]) ) text(labels, heights[heights < maxh*0.8], heights[heights < maxh*0.8], pos = 3, offset = 0.2, cex = 1.5)
    text(xbar[heights >= maxh*0.8], heights[heights >= maxh*0.8], heights[heights >= maxh*0.8], pos = 1, offset = 0.4, cex = 1.5)
    #print( qplot(data = dfcol, x=dfcol[[1]], main = paste('QA for',s_data)) + theme_bw() )
    #legend("topright", legend = summary(dfcol), cex = 1, bty="n")
  }
}

addplotSummary <- function (dfcol, outliers = F) {
  usr <- par("usr")
  wh <- diff(usr)[c(1,3)]
  xy <- usr[c(1,3)]
  offset_text = 1.8
  if (outliers) {
    text(0.90*wh[1]+xy[1], usr[4], 'OUTLIERS', pos = 1, offset = 0.5, col = 'red', cex = 1.2)
    offset_text = 3
  }
  text(0.90*wh[1]+xy[1], usr[4], paste(summary(dfcol), collapse="\n"), pos=1, offset = offset_text) #adj=c(1,1))  #pos=2, offset = c(5))
}

addplotNAData <- function(data, type = 'small', third = NA) {
  oldpar <- par(no.readonly=T)
  if (type=='small') {
    figpos <- ifelse(rep(all(is.na(third)),4), c(0.89,0.99,0.50,0.74), c(0.80,1,0.75,0.9))# c(0.72,0.82,0.75,0.99), c(0.80,1,0.75,0.9))
  }
  else if (type=='big') {
#     figpos <- c(0.75,0.92,0.1,0.9)  # coordinates in scaled units
    figpos <- c(0.65,0.82,0.1,0.9)  # coordinates in scaled units
  }
  else e("wrong 'type=' for addplotNAData")
  par(omd = get_omd(), mai = rep(0,4), fig = figpos, new = T)
  if (all(is.na(third)))
    xbar <- plot(factor(is.na(data), levels = c(T,F), labels = c("NA", "DATA") ))
  else
    xbar <- plot(factor(is.na(data) + is.na(third), levels = c(0,1,2), labels = c("DISP", "UNPARSED", "NA") ))
  par(oldpar)
}

get_omd <- function() { # gives the omd corresponding to the existing margins
  offmd <- rep(0,4)
  offmd[c(1,3)] <- par('mai')[c(2,1)] / dev.size() # x1, y1 form left and bottom # Several units possible, btw : dev.size(units = c("in", "cm", "px"))
  offmd[c(2,4)] <- 1 - par('mai')[c(4,3)] / dev.size() # x2, y2, from right and top
  return(offmd)
}

require(lubridate)
parse_date <- function (cvec) {suppressWarnings({
  out <- NA
  index <- ! is.na(cvec) # remove these
  out[index] <- sapply(cvec[index], mdy, USE.NAMES = FALSE )
  out[is.na(out) & index] <- sapply(cvec[is.na(out) & index], function(x) dmy(paste0("01-", x)), USE.NAMES = FALSE )
  out[is.na(out) & index] <- sapply(cvec[is.na(out) & index], function(x) dmy(paste0("15-06-", x)), USE.NAMES = FALSE )
  class(out) <- 'POSIXct'
  tz(out) <- 'UTC'
  result<- as.Date(out)
})
}

#######

