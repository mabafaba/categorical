
#' cat_categorical is the main vector class. It gives a general structure from which the more specific categorical classes can be constructed.
#' cat_categorical is:
#'    - a standard vctrs_vctr vctrs_rcrd (see vctrs package), with a logical vector for each level, indicating which levels were selected for each entry
#'    - the 'main' values are a character strings
#'    - it has a 'levels' attribute with a vector containing all unique categories
#'    - it has an attribute for _alternative values_, allowing the user to add different alternative values, such as labels in different languages
#'    - it has an attribute for _internal alternative values_ intended for use in specific subclasses. What these are depends on the specific sublass; for example these could be:
#'        - for an "interval" subclass: lower and upper limit
#'        - for an "ordinal" subclass: integer rank

#' it follows the generic methods to create categorical() vectors from different data types.
#' for most types, we
#' - check if the inputs are valid
#' - convert the input to a logical matrix
#' - then use categorical.matrix(), which
#'   - checks that the inputs are still valid
#'   - converts inputs to what they should be like for the final attributes
#'   - call new_categorical()
#'     - which creates the final vector with vctrs::new_rcrd()

#' create a new categorical variable
#'
#' @param x a vector or list to be used as values for the categorical vector
#' @param levels list of possible values for x; similar to factor levels
#' @param alternatives a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{\link{alternate}}.
#' @param alternatives_internal a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{alternate}. "internal" alternatives are used to store 'fixed' alternatives for classes extending 'cat_categorical'.
#' @param class optional: name the subclass if you are using this to define a new vector type on top of the 'cat_categorical' class
#' @importFrom vctrs vec_ptype
#' @importFrom vctrs vec_ptype2
#' @importFrom vctrs vec_ptype2.character
#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_cast.character
#' @export
categorical <- function(x = logical(),
                        levels = NULL,
                        alternatives = empty_alternatives(levels),
                        alternatives_internal = empty_alternatives(levels),
                        class = c()) {
  UseMethod("categorical")
}


#' create a new categorical variable
#' @inheritParams categorical
#' @param x a logical matrix indicating which levels are selected per record (each row is a record, each column corresponds to a level specified in 'level's)
new_categorical <- function(x = logical(), levels,
                            alternatives_internal = empty_alternatives(levels),
                            alternatives = empty_alternatives(levels),
                            class = c()) {



  if(any(is.na(levels))){
    stop("levels can not be NA")
  }

  if(any(duplicated(levels))){
    stop("levels must be unique")
  }

  levels <- vctrs::vec_cast(levels,character())
  if(any(duplicated(levels))){
    stop("levels must be unique when converted to characters")
  }

  # consistent NAs:
  x[apply(x,1,function(x){any(is.na(x))}),]<-NA


  logical_fields<- as.list(as.data.frame(x))
  names(logical_fields)<-levels
  if(length(logical_fields)==0 & length(levels)==0){
    logical_fields<-list('0'=logical())
  }


  if(!is.matrix(x)){stop("x must be a matrix")}
  if(!is.logical(x)){stop("x must be logical")}




  vctrs::new_rcrd(fields = logical_fields,
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = alternatives,
                  class = c(class, "cat_categorical"))

}



#' categorical constructors check inputs, convert to matrix and finally should call this function.
#' @inheritParams categorical
#' @param x a logical matrix; each row corresponding to a value in the resulting vector, each column to a level / category
#' @export
categorical.matrix<-function(x = logical(),
                             levels,
                             alternatives = empty_alternatives(levels),
                             alternatives_internal = empty_alternatives(levels),
                             class = c()){

  if(!is.matrix(x)){stop("x must be a matrix")}
  if(!is.logical(x)){stop("to make a categorical from a matrix, the matrix must be logical")}
  if(!(ncol(x)==length(levels))){
    stop("to make a categorical vector from a logical matrix, you must provide levels, and the matrix must have one column per level")
  }


  # matrix to list of logical vectors, named after levels:
  logical_fields<-   as.list(as.data.frame(x))
  names(logical_fields)<-levels


  public_alternatives<-alternatives
  if(length(public_alternatives)==0){
    public_alternatives<-NULL
  }
  # remove nulls from alternatives

  # make sure alternatives have same length, and match levels:

  public_alternatives<-enforce_alternative_lengths_match_levels(public_alternatives,levels)
  alternatives_internal<-enforce_alternative_lengths_match_levels(alternatives_internal,levels)

  if(all(purrr::map_lgl(alternatives_internal,is.null))){
    alternatives_internal<-empty_alternatives(levels)
  }else{
    alternative_lengths<-purrr::map_int(alternatives_internal,length)
    bad_length<-(alternative_lengths != length(levels))
    if(any(bad_length)){
      stop(paste('internal alternative(s) with wrong length (must be same length as levels):',
                 paste(names(public_alternatives)[bad_length],collapse = " ")))
    }
  }
  if(all(purrr::map_lgl(public_alternatives,is.null))){
    public_alternatives<-empty_alternatives(levels)
  }else{
    alternative_lengths<-purrr::map_int(public_alternatives,length)
    bad_length<-(alternative_lengths != length(levels))
    if(any(bad_length)){
      stop(paste('alternative(s) with wrong length (must be same length as levels):',
                 paste(names(public_alternatives)[bad_length],collapse = " ")))
    }
  }







  # alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  if(is.null(alternatives_internal)){
    alternatives_internal<-empty_alternatives(levels)
  }

  alternatives_internal<-tibble::as_tibble(alternatives_internal, .rows = length(levels))

  # public alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  public_alternatives<-tibble::as_tibble(public_alternatives,.rows = length(levels))

  # all is goood, let's go make a new categorical wohay:
  new_categorical(x = x,
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = public_alternatives,
                  class = class)

}






#' create a categorical variable from categorical input
#' @inheritParams categorical
#' @param x a categorical vector
#' @export
categorical.cat_categorical <- function(x = logical(),
                                        levels = NULL,
                                        alternatives = NULL,
                                        alternatives_internal = NULL,
                                        class = c()){

  if(is.null(levels)){
    levels <- levels(x)
  }


  new<-categorical.matrix(as.matrix(x),
                          levels=levels,
                          alternatives = alternatives,
                          alternatives_internal = alternatives_internal,
                          class = class)


  common <- vec_ptype2.cat_categorical.cat_categorical(x,new)
  vec_cast(x,common)
}

#' @export
categorical.default <- function(x = logical(),
                                levels = unique_and_not_na(unlist(x)),
                                alternatives = empty_alternatives(levels),
                                alternatives_internal = empty_alternatives(levels),
                                class = c()) {

  # if x is of length 0, we take a short cut (empty matrix if no levels provided, matrix with no rows and length(levels) columns if levels provided):
  if(length(x)==0){
    if(length(levels)==0){
      logical_fields<-matrix(logical(0), nrow = 0, ncol = 0)

    }else{
      logical_fields<-do.call(cbind, purrr::map(levels,function(x){logical(0)}))
    }

    return(categorical.matrix(logical_fields,
                              levels = levels,
                              alternatives = alternatives,
                              alternatives_internal = alternatives_internal,
                              class = class)
    )

  }


  if(length(levels)==0 & length(x)!=0){
    stop("a categorical vector with no levels can not have any values (not even NA)")
  }


  # by default, we assume we're dealing with a vector or a list of values (for multiple)

  # if vector, make a list so we have only one case to deal with: values should always be a list in the end (unless it's a logical matrix)


  assertthat::assert_that(all(unique_and_not_na(unlist(x,use.names = FALSE)) %in% levels))

  if(!is.list(x)){
    x<-as.list(x)
  }
  # all values in the list should be exist in the levels:

  # make a logical matrix:
  logical_fields<-as.matrix(
    do.call(rbind,
            purrr::map(x,function(x){
              levels %in% x
            })
    )
  )

  logical_fields[purrr::map_lgl(x,function(x){any(is.na(x))}),]<-NA
  categorical.matrix(logical_fields,
                     levels = levels,
                     alternatives = alternatives,
                     alternatives_internal = alternatives_internal,
                     class)

}

#' convert to categorical variable
#'
#' @inheritParams categorical
#' @export
as_categorical<-categorical


# #' find superficial NAs
# #' @param x a <categorical> vectors
# #' @details "superficial NA's" appear in categorical vectors where the levels themselves are not NA, but the alternative has no value for the level
# superficial_nas<-function(x){

#   active_values <-get_active_values(x)
#   level_values  <-get_level_values(x)

#   superficial_nas<- is.na(active_values) & !is.na(level_values)
#   names(superficial_nas)<-level_values
#   return(superficial_nas)
# }



# basic type functions

#' check if vector is of class cat_categorical
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is_categorical<-function(x){
  inherits(x,'cat_categorical')
}

# basic type functions

#' check if vector is of class cat_categorical
#' @param x a vector
#' @return TRUE if it is a categorical vector
#' @export
is.categorical<-is_categorical

#' Mutate categorical type variables in a data frame
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a categorical column. Each row's value is a vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
mutate_categorical<-function(.data, ...){
  mutation <- rlang::enquos(...)
  .data<-dplyr::mutate( dplyr::rowwise(.data), !!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# following the vctrs vignette, adding this wihtout knowing why or what it does:

#' @importFrom methods setOldClass
methods::setOldClass(c("cat_categorical", "vctrs_vctr"))








#' mutate categorical type variables, while treating each choice as logical
#'
#' This is much simpler than it sounds & useful; needs better description & name
#' @param x a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a categorical column. Each row's value is a vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
categorical_logic<-function(x,...){
  mutation <- rlang::enquos(...)

  .data<- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
  .data<-dplyr::transmute(dplyr::rowwise(.data), !!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  unname(unlist(.data))
}








has_multiple_response<-function(x){
  if(!is_categorical(x)){stop('not a categorical vector')}
  count_selected <- apply(as.matrix(x),1,function(x){sum(x)})
  any_record_not_na_and_not_selected_exactly_one <- any(count_selected[!is.na(count_selected)])!=1
  return(any_record_not_na_and_not_selected_exactly_one)
}



#' take unique values from a vector and remove all NAs
#' @param x vector
unique_and_not_na<-function(x){
  x<-unique(x)
  x<-x[!is.na(x)]
  x
}


empty_alternatives<-function(levels){
  tibble::tibble(.rows = length(levels))
}
