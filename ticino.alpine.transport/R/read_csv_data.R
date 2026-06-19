#' @title TODO
#' @param TODO
#' @description
#' TODO
#'
#' @returns TODO
#' @export
read_csv_data <- function() {
  data <- read.csv("SwissCities.csv", sep = ",")
  
  group <- data[data$group_id == '4',]
  
  return(group)
}