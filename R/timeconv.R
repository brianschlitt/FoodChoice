#' @title Timeconv
#' 
#' @description Takes input as column of dataframe showing trial times in hh:mm format and converts to decimal number of hours for easier further quantatative analyses.
#' @param time_column Data frame column containing trial times in hh:mm format
#' @keywords trial time convert decimal
#' @export
#' @examples 
#' timeconv(sampledata$Time)


timeconv <- function(time_column) {
  time_column <- (as.numeric(gsub(":.*", "", time_column)))+((as.numeric(gsub(".*:", "", time_column)))/60)
  return(time_column)
}
  
