
#' create a new interval variable
#'
#' @param x a vector of to be used as values for the interval vector. These should be characters for most use cases (but can be other types)
#' @param ranks a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{alternate}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
new_interval<-function(lower, upper, closed = c(TRUE,FALSE),levels,levels_lower,levels_upper, ...){


  interval_matrix<-cbind(lower,upper)

  levels_matrix<-unique(interval)

  start_interval<-function(){
    ifelse(closed[1],'[','(')
  }

  end_interval<-function(){
    ifelse(closed[2],']',')')
  }

  interval_matrix_to_levels<-function(x){
  levels<-apply(x,1,function(x){
    paste0(start_interval(),x[1],', ',x[2],end_interval())
  })
  }

  levels<-interval_matrix_to_levels(levels_matrix)

  alternatives_internal<-tibble(lower=levels_matrix[,1],
                       upper=levels_matrix[,2],
                       lower_closed = rep(closed[1],nrow(levels_matrix)),
                       upper_closed = rep(closed[1],nrow(levels_matrix))
  )

  x<-interval_matrix_to_levels(interval_matrix)
  x<-categorical(x,levels = levels, alternatives_internal = alternatives_internal)
  class(x)<-c('cat_interval', class(x))
  class(x)<-class(x)[class(x)!='cat_categorical']
  x
}



#' create a new interval variable
#'
#' @param x a vector of to be used as values for the interval vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param rank a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{alternate}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
interval <- function(x, levels = unique(unlist(x)), ranks = 1:length(levels), ...) {
  assertthat::assert_that(is.numeric(ranks))

  # all is goood, let's go make a new categorical wohay:
  x <- new_interval(x = x,
                   levels = levels,
                   ranks=ranks,
                   ...
  )
}


#' create a new interval variable
#'
#' @param x a vector of to be used as values for the interval vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param rank a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{interval_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
as_interval<-interval


# basic type functions

#' check if vector is of class cat_interval
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is_interval<-function(x){
  inherits(x,'cat_interval')
}

# basic type functions

#' check if vector is of class cat_interval
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is.interval<-is_interval

#' Mutate interval type variables in a data frame
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a interval column. Each row's value is a vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
mutate_categorical<-function(.data,...){
  mutation <- rlang::enquos(...)
  .data<-.data %>% dplyr::rowwise %>% dplyr::mutate(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# following the vctrs vignette, adding this wihtout knowing why or what it does:

#' @importFrom methods setOldClass
methods::setOldClass(c("cat_interval", "vctrs_vctr"))
