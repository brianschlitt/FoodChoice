#' @title Finalplots
#' 
#' @description Takes input as dataframe containing final locations as well as denotations for species identities, returns final location statistics and bar plot.
#' @param dfname name of data frame
#' @param final_loc_colname name of column in data frame
#' @param species1 denotation of plant species 1 being tested
#' @param species2 denotation of plant species 2 being tested
#' @param species3 denotation of plant species 3 being tested
#' @keywords food choice final location
#' @note Please format final_loc_colname as dataframe$colname, and be sure to include quotes around each species denotation.
#' @export
#' @examples 
#' finalplots(sampledata, sampledata$Final.Location, "A", "P", "V")


finalplots <- function(dfname, final_loc_colname, species1, species2, species3){
  
  sp1pct <- (sum(final_loc_colname==species1))/(length(final_loc_colname))*100
  sp2pct <- (sum(final_loc_colname==species2))/(length(final_loc_colname))*100
  sp3pct <- (sum(final_loc_colname==species3))/(length(final_loc_colname))*100
  print(sprintf("%f percent ended up on species 1", sp1pct))
  print(sprintf("%f percent ended up on species 2", sp2pct))
  print(sprintf("%f percent ended up on species 3", sp3pct))
  
  library(ggplot2)
  ggplot(data = dfname, aes(x = final_loc_colname)) +
    geom_bar(fill="darkblue")
}
