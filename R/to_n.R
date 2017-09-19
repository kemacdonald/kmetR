#' Convert variable to numeric
#'
#' @param x A string to be converted
#' @return A numeric
#' @examples
#' to_n("10")

to_n <- function (x) {
  as.numeric(as.character(x))
}
