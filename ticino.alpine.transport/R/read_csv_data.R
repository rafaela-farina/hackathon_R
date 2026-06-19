#' @title TODO
#' @param TODO
#' @description
#' TODO
#'
#' @returns TODO
#' @export
read_csv_data <- function() {
  csv_path <- system.file(
    "extdata",
    "SwissCities.csv",
    package = "ticino.alpine.transport"
  )

  data <- read.csv(csv_path, sep = ",")

  group <- data[data$group_id == '4',]

  return(group)
}
