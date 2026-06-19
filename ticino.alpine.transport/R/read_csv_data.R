#' @title Read saved CSV data
#' @description
#' Returns the saved dataset included in the ticino.alpine.transport package.
#'
#' @return The saved dataset from ticino.alpine.transport.
#' @export
read_csv_data <- function() {
  return(ticino.alpine.transport::saved_data)
}

