#' # dsdist
#' @author Gürol Canbek, <gurol44@gmail.com>  
#' Copyright (C) 2017-2018 Gürol CANBEK  
#' @references <http://gurol.canbek.com>  
#' @keywords utilities, common functions  
#' @title utils - Common utility R functions  
#' @date 16 January 2017  
#' @version 1.2  
#' @note version history  
#' 1.2, 16 Feburary 2018, Plot to devide or PNG file
#' 1.1, 14 February 2018, Column name checking avoiding parameter for rclip
#' 1.0, 1 February 2017, The first version  
#' @description Common R functions that can be called from other scripts

#' libraries  
library(parallel) # Preinstalled in environment

#' ### getNumberOfCPUCores
#' Return the number of CPU cores in the current host  
#' **Parameters:**  
#' *logical*: if possible, use the number of physical CPUs/cores (if FALSE) (default: FALSE)  
#' **Return:**  
#' Number of CPU cores  
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

#' ### rclip
#' Read from the Clipboard (i.e. Paste)  
#' **Parameters:**  
#' *sep*: Seperator between column values (default: TAB)  
#' *na*:  Not Available identifier (default: 'NA')  
#' *dec*: Decimal seperator (default: '.')  
#' *header*: Does source have column names (header)? (default: TRUE)  
#' *stringsAsFactors*:Should character vectors be converted to factors? (default: FALSE)  
#' *check.names*: Avoid addition of "X" prefix into column names (default: FALSE)
#' **Return:**  
#' Readed data frame  
#' **Details:**  
#' Code changes according to operating system (Windows or Mac OS)  
#' **Warning:**  
#' ignore warning message: incomplete final line found by readTableHeader on 'pbpaste'
#' **Examples:** `ACC <- rclip()` or `ACC <- wclip(dec= ',')`  
rclip <- function(sep='\t', na='NA', dec='.', header=TRUE,
                  stringsAsFactors=FALSE, check.names=FALSE)
{
  if (.Platform$OS.type == 'windows')
    values <- read.table('clipboard-256', sep=sep, dec=dec, header=header,
                         stringsAsFactors=stringsAsFactors,
                         check.names=check.names)
  else {
    clip <- pipe('pbpaste')
    values <- read.table(file=clip, sep=sep, na=na, dec=dec, header=header,
                         stringsAsFactors=stringsAsFactors,
                         check.names=check.names)
  }
  
  return(values)
}


#' ### pressEnterToContinue
#' Stop script run and show a (custom) message to user to press ENTER  
#' **Parameters:**  
#' *message*: custom message text to display (default: '')  
#' **Return:**  
#' none  
#' **Details:**  
#' Show a given message with 'Press [enter] to continue' statement and wait for  
#' the user interaction. It is useful for pausing script run  
#' **Examples:** `pressEnterToContinue()` or `pressEnterToContinue('wait')`  
pressEnterToContinue<-function(message='')
{
  invisible(readline(prompt=paste0(message, 'Press [enter] to continue')))
}

#' ### renameDataFrameColumn
#' Rename the column name of a data frame  
#' **Parameters:**  
#' *df*: data frame  
#' *column_name*: existing column name  
#' *new_column_name*: new column name  
#' **Return:**  
#' new data frame  
#' **Details:**  
#' **Examples:** `renameDataFrameColumn(df, 'test', 'product')`  
renameDataFrameColumn<-function(df, column_name, new_column_name)
{
  colnames(df)[colnames(df)==column_name] <- new_column_name
  
  return(df)
}

#' ### plotToDeviceOrFile
#' Plot a graphic to a device of a PNG file (if a file name is given)  
#' **Parameters:**  
#' *data*: data or R object to plot  
#' *main_title*: n overall title for the plot (default: NULL)  
#' *xlabel*: a title for the x axis (default: NULL)  
#' *ylabel*: a title for the x axis (default: NULL)  
#' *filepath*: path of the file (default: NULL to plot on device)  
#' *width*: the width of the device/file (default: 20cm)  
#' *height*: the height of the device/file (default: 15cm)  
#' *units*: The units in which height and width are given (default: 'cm')
#'          Could be 'in', 'px', 'mm'  
#' *res*: The nominal resolution in ppi which will be recorded in the bitmap file (default: 300)  
#' **Return:**  
#' none  
#' **Examples:** `plotToDeviceOrFile(data, filepath='fig1.png')`  
plotToDeviceOrFile<-function(data, col=NULL,
                             main_title=NULL, xlabel=NULL, ylabel=NULL,
                             filepath=NULL, width=20, height=15, units='cm',
                             res=300)
{
  if (FALSE == is.null(filepath)) {
    png(filename=filepath, witdth=width, height=height, units=units, res=res)
  }
  plot(data, col=col, main=main_title, xlab=xlabel)
  if (FALSE == is.null(filepath)) {
    dev.off()
  }
}