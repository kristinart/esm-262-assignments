#' Summarize Vector of Fish Catch Data
#' 
#' This function determines the most common fish, most rare fish, and total number of fish per fish catch
#' @param fish_data (vector of fish counts from one fish catch)
#' @return fish catch summary (most common fish, most rare fish, total fish count)
#' 
# function definition
summarize_fish_catch = function(fish_data){
  # check if the data input is in vector form 
  if(!is.vector(fish_data)){
    stop("Data input into this function is not in vector format.")
  }
  
  #find the fish that is the most common
  most_common <- names(which.max(summary(as.factor(fish_data))))
  
  #find the fish that is the most rare
  most_rare <- names(which.min(summary(as.factor((fish_data)))))
  
  #find the total number of fish
  total_fish <- length(fish_data)
  
  #return outputs as a list
  return(list('most common fish:' = most_common, 'most rare fish:' = most_rare, 'total number of fish caught:' = total_fish))
}
