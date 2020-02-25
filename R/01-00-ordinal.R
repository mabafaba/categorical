
#' create a new ordinal variable
#'
#' @param x a vector of to be used as values for the ordinal vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param ranks a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{alternate}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
new_ordinal<-function(x, levels, ranks, ...){
  x<-categorical(x = x,levels = levels, alternatives_internal = list(ranks=ranks), class = 'cat_ordinal')
  x
}



#' create a new ordinal variable
#'
#' @param x a vector of to be used as values for the ordinal vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param rank a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{alternate}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
ordinal <- function(x, levels = unique(unlist(x)), ranks = 1:length(levels), ...) {
  assertthat::assert_that(is.numeric(ranks))

  # all is goood, let's go make a new categorical wohay:
  x <- new_ordinal(x = x,
                   levels = levels,
                   ranks=ranks,
                   ...
  )
}


#' create a new ordinal variable
#'
#' @inheritParams ordinal
#' @export
as_ordinal<-ordinal


# basic type functions

#' check if vector is of class cat_ordinal
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is_ordinal<-function(x){
  inherits(x,'cat_ordinal')
}

# basic type functions

#' check if vector is of class cat_ordinal
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is.ordinal<-is_ordinal


# following the vctrs vignette, adding this wihtout knowing why or what it does:

#' @importFrom methods setOldClass
methods::setOldClass(c("cat_ordinal", "vctrs_vctr"))
