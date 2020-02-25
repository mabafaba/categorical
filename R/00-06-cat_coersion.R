unused_colname<-'____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__'

#' When the change happens implicitly (e.g in c()) we call it coercion
#' vec_ptype2(x, y) defines possible set of coercions.
#' It returns a prototype if x and y can be safely coerced to the same prototype;
#' otherwise it returns an error.
#' The set of automatic coercions is usually quite small
#' because too many tend to make code harder to reason about
#' and silently propagate mistakes.
#'
#'
#' each combo of our class and other classes that can be coerced together has
#' a function that returns just a prototype of the resulting class
#'
#' namespace exports:
#' generic double dispatch boilerplate like this:
#' #' @method vec_cast CLASS
#' #' @export
#' #' @export vec_cast.CLASS
#' vec_cast.vctrs_percent <- function(x, to, ...) {
#' }
#'
#' individual dispatches like this:
#'
#' #' @method vec_cast.CLASS1 CLASS2
#' #' @export


#' cat_categorical
#' @param x categorical vector
#' @method vec_ptype cat_categorical
#' @export
vec_ptype.cat_categorical<-function(x) x[0]
