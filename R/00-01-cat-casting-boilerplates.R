
#' BOILERPLATES
#'
#' Based on browseVignettes('vctrs')
#'
#' start with the boilerplate for vec_ptype2()
#' this is just so later we can write vec_ptype2.cat_categorical.OTHERCLASS
#' to define how these two classes should be coerced together
#' @param x vector object
#' @param y vector object
#' @param ... additional arguments
#' @method vec_ptype2 cat_categorical
#' @export
#' @export vec_ptype2.cat_categorical
vec_ptype2.cat_categorical <- function(x, y, ...) UseMethod("vec_ptype2.cat_categorical", y)
vec_ptype2.cat_categorical.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' cast categorical vectors
#' @param x vector
#' @param to prototype to conver to
#' @param ... additional arguments
#' @method vec_cast cat_categorical
#' @export
#' @export vec_cast.cat_categorical
vec_cast.cat_categorical <- function(x, to, ...) UseMethod("vec_cast.cat_categorical")
vec_cast.cat_categorical.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

