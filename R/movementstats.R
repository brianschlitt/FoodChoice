#' @title Movementstats
#' 
#' @description Takes input as dataframe containing inital placement, check 1, check 2, check 3, check 4, and final locations for a number of food choice trials and returns statistics on how frequently movement occurred between checks.
#' @param placement_column column of data frame containing locations placement locations
#' @param check1 column of data frame containing locations at check 1
#' @param check2 column of data frame containing locations at check 2
#' @param check3 column of data frame containing locations at check 3
#' @param check4 column of data frame containing locations at check 4
#' @param final_loc column of data frame containing locations after full trial time elapsed
#' @keywords food choice movement
#' @export
#' @examples 
#' movementstats(sampledata$Placement, sampledata$Check.1, sampledata$Check.2, sampledata$Check.3, sampledata$Check.4, sampledata$Final.Location)


movementstats <- function(placement_column, check1, check2, check3, check4, final_loc){
  move1 <- 0
  move2 <- 0
  move3 <- 0
  move4 <- 0
  finalmove <- 0
  for(i in 1:length(placement_column)){
    if(placement_column[i]==check1[i]){
      move1 <- move1 + 1
      
    }
    if(placement_column[i]==check1[i] & placement_column[i]==check2[i]){
      move2 <- move2 + 1
      
    }
    if(placement_column[i]==check1[i] & placement_column[i]==check2[i] & placement_column[i]==check3[i]){
      move3 <- move3 + 1
      move3pct <- move3/length(placement_column)
    }
    if(placement_column[i]==check1[i] & placement_column[i]==check2[i] & placement_column[i]==check3[i] & placement_column[i]==check4[i]){
      move4 <- move4 + 1
      
    }
    if(placement_column[i]==check1[i] & placement_column[i]==check2[i] & placement_column[i]==check3[i] & placement_column[i]==check4[i] & placement_column[i]==final_loc[i]){
      finalmove <- finalmove +1
      
    }
  }
  move1pct <- (move1/length(placement_column))*100
  move2pct <- (move2/length(placement_column))*100
  move3pct <- (move3/length(placement_column))*100
  move4pct <- (move4/length(placement_column))*100
  finalpct <- (finalmove/length(placement_column))*100
  print(sprintf("%f percent of trials saw no movement from initial placement to check 1", move1pct))
  print(sprintf("%f percent of trials saw no movement from initial placement to check 2", move2pct))
  print(sprintf("%f percent of trials saw no movement from initial placement to check 3", move3pct))
  print(sprintf("%f percent of trials saw no movement from initial placement to check 4", move4pct))
  print(sprintf("%f percent of trials saw no movement from initial placement to final location", finalpct))
  
}


