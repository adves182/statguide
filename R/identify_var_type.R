#' Identify variable type
#'
#' Determines whether a variable is numeric or categorical.
#'
#' @param x A single variable (vector), such as `data$column`.
#'
#' @return A character string: "numeric" or "categorical".
#'
#' @examples
#' identify_var_type(1:10)
#' identify_var_type(factor(c("A", "B")))
#' identify_var_type(PlantGrowth$weight)
#' identify_var_type(PlantGrowth$group)
#'
#' @export
identify_var_type <- function(x) {
  if (is.numeric(x)) return("numeric")
  if (is.factor(x) || is.character(x)) return("categorical")
  stop("Unsupported variable type.")
}
