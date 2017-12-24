library(parallel)

getNumberOfCPUCores<-function(logical=FALSE)
{
  cores <- detectCores(logical=logical)
  
  if (is.na(cores))
    cores <- 1
  
  return(cores)
}

#' ### wclip
#' Write to the Clipboard (i.e. Copy)  
#' **Parameters:**  
#' *metric*: Performance metric  
#' *sep*: Seperator between column values (default: TAB)  
#' *na*:  Not Available identifies (default: 'NA')  
#' *dec*: Decimal seperator (default: '.')  
#' *row.names*: Does source metric have row names (default: TRUE)  
#' *col.names*: Does source metric have column names (default: TRUE)  
#' **Return:**  
#' none  
#' **Details:**  
#' Code changes according to operating system (Windows or Mac OS)  
#' **Warning:**  
#' write.table writes unwanted leading empty column to header when has rownames  
#' See http://stackoverflow.com/questions/2478352/write-table-writes-unwanted-leading-empty-column-to-header-when-has-rownames  
#' **Examples:** `wclip(ACC)` or `wclip(ACC, dec= ',')`  
wclip <- function(metric, sep='\t', na='NA', dec='.',
                  row.names=TRUE, col.names=TRUE)
{
  if (.Platform$OS.type == 'windows')
    write.table(metric, 'clipboard-256', sep=sep, dec=dec,
                row.names=row.names, col.names=col.names)
  else {
    clip <- pipe('pbcopy', 'w')
    write.table(metric, file=clip, sep=sep, na=na, dec=dec,
                row.names=row.names, col.names=col.names)
    close(clip)
  }
}

rclip <- function(sep='\t', na='NA', dec='.', header=TRUE)
{
  if (.Platform$OS.type == 'windows')
    values <- read.table('clipboard-256', sep=sep, dec=dec, header=header)
  else {
    clip <- pipe('pbpaste')
    values <- read.table(file=clip, sep=sep, na=na, dec=dec, header=header)
  }
  
  return(values)
}

#' Stop script run and show a (custom) message to user to press ENTER
#'
#' Show a given message with 'Press [enter] to continue' statement and wait for
#' the user interaction. It is useful for pausing script run
#'
#' @param message custom message text to display
pressEnterToContinue<-function(message='')
{
  invisible(readline(prompt=paste0(message, 'Press [enter] to continue')))
}

renameDataFrameColumn<-function(df, column_name, new_column_name)
{
  colnames(df)[colnames(df)==column_name] <- new_column_name
  
  return(df)
}