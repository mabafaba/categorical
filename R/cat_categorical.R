


#' cat_categorical is the meta class that gives the general structure for the more specific categorical classes that extend it.
#' cat_categorical is:
#'    - a vctrs_vctr
#'    - values are generally stored as a list (to allow select multiple and other more complex subclasses)
#'    - it has an attribute for levels / allowed values.
#'    - it has an attribute for _closed alternative values_. what these are depends on the specific sublass; for example these could be:
#'        - character labels (select)
#'        - integer rank (ordinal)
#'    - it has an attribute for _open alternative values_, allowing the user to add different alternative values, such as labels in different languages
#'

#' create a new categorical variable
#'
#' @param x a vector or list to be used as values for the categorical vector
#' @param levels list of possible values for x; similar to factor levels
#' @param alternatives_internal a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. "internal" alternatives are used to store 'fixed' alternatives for classes extending 'cat_categorical'.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_ptype
#' @importFrom vctrs vec_ptype2
#' @export
categorical <- function(x = logical(),
                        levels = unique_and_not_na(unlist(x)),
                        alternatives = empty_alternatives(levels),
                        alternatives_internal = empty_alternatives(levels),
                        active_alternative = NULL,
                        active_alternative_is_internal = FALSE,
                        class = c()) {
  UseMethod("categorical")
}


#' create a categorical variable from categorical input
#' @export
categorical.categorical <- function(x = logical(),
                                    levels = levels(x),
                                    alternatives_internal = NULL,
                                    alternatives = NULL,
                                    active_alternative = NULL,
                                    active_alternative_is_internal = FALSE,
                                    class = c()){

  # TODO: alternatives argument to this function not used (unclear what the expected behaviour should be here)
  #       unclear whether those arguments can safely be removed
  new<-new_categorical(mr_logical_matrix(x),
                       levels=levels(x),
                       alternatives = alternatives(x,F),
                       alternatives_internal = alternatives(x,T),
                       active_alternative = active_alternative,
                       active_alternative_is_internal = active_alternative_is_internal,
                       class = class)


  common <- vec_ptype2.cat_categorical.cat_categorical(x,new)
  vec_cast(x,common)
}

#' @export
categorical.default <- function(x = logical(),
                                levels = unique_and_not_na(unlist(x)),
                                alternatives = empty_alternatives(levels),
                                alternatives_internal = empty_alternatives(levels),
                                active_alternative = NULL,
                                active_alternative_is_internal = FALSE,
                                class = c()) {

  # if x is of length 0, we take a short cut (empty matrix if no levels provided, matrix with no rows and length(levels) columns if levels provided):
  if(length(x)==0){
      if(length(levels)==0){
        logical_fields<-matrix(logical(0), nrow = 0, ncol = 0)

      }else{
        logical_fields<-purrr::map(levels,function(x){logical(0)}) %>% do.call(cbind,.)
      }

      return(categorical.matrix(logical_fields,
             levels = levels,
             alternatives = alternatives,
             alternatives_internal = alternatives_internal,
             active_alternative = active_alternative,
             active_alternative_is_internal = active_alternative_is_internal,
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
  logical_fields<-purrr::map(x,function(x){
    levels %in% x
  }) %>% do.call(rbind,.) %>% as.matrix

  logical_fields[purrr::map_lgl(x,function(x){any(is.na(x))}),]<-NA
  categorical.matrix(logical_fields,
                     levels = levels,
                     alternatives = alternatives,
                     alternatives_internal = alternatives_internal,
                     active_alternative = active_alternative,
                     active_alternative_is_internal = active_alternative_is_internal,
                     class)

}

#' categorical constructors check inputs, convert to matrix and finally should call this function.
#' @export
categorical.matrix<-function(x = logical(),
                             levels,
                             alternatives = empty_alternatives(levels),
                             alternatives_internal = empty_alternatives(levels),
                             active_alternative = NULL,
                             active_alternative_is_internal = FALSE,
                             class = c()){

  if(!is.matrix(x)){stop("x must be a matrix")}
  if(!is.logical(x)){stop("to make a categorical from a matrix, the matrix must be logical")}
  if(!(ncol(x)==length(levels))){
    stop("to make a categorical vector from a logical matrix, you must provide levels, and the matrix must have one column per level")
  }


    # matrix to list of logical vectors, named after levels:
    logical_fields<- x %>% as.data.frame %>% as.list
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
                  active_alternative = active_alternative,
                  active_alternative_is_internal = active_alternative_is_internal,
                  class = class)

}





enforce_alternative_lengths_match_levels<-function(alternatives,levels){

  if(is.null(alternatives)){return(alternatives)}

  if(is.data.frame(alternatives)){
    if(!nrow(alternatives)==length(levels)){
      stop("all provided alternatives must have exactly one value per categorical level")
    }
    return(alternatives)
  }

  if(is.list(alternatives)){
    # remove empty elements
    alternatives[sapply(alternatives, is.null)] <- NULL
    if(length(alternatives)==0){NULL}
    # stop if not all remaining have the same length as levels
    if(!all(purrr::map_dbl(alternatives,vctrs::vec_size) == length(levels))){stop("all provided alternatives must have exactly one value per categorical level")}
    return(alternatives)
  }
 # for data frames, check nrow equals length levels:



  stop("alternatives must be a list or a data.frame")
}



#' @method levels cat_categorical
#' @S3method levels cat_categorical
levels.cat_categorical<-function(x){
  attr(x,'levels')
}


#' create a new categorical variable
#'
#' @param x a logical matrix indicating which levels are selected per record (each row is a record, each column corresponds to a level specified in 'level's)
#' @param levels vector of possible values for x; similar to factor levels. Defaults to the unique values in x. Will be converted to characters
#' @param alternatives_internal a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. "internal" alternatives are used to store 'fixed' alternatives for classes extending 'cat_categorical'.
#' @param alternatives a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
new_categorical <- function(x = logical(), levels,
                            alternatives_internal = empty_alternatives(levels),
                            alternatives = empty_alternatives(levels),
                            active_alternative = NULL,
                            active_alternative_is_internal = FALSE,
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


  logical_fields<-x %>% as.data.frame %>% as.list
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
                  active_alternative = active_alternative,
                  active_alternative_is_internal = active_alternative_is_internal,
                  # multiple_selection = multiple_selection,
                  class = c(class, "cat_categorical"))

}


#' create a new categorical variable
#'
#' @param x a vector or list to be used as values for the categorical vector
#' @param levels list of possible values for x; similar to factor levels
#' @param alternatives_internal a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. "internal" alternatives are used to store 'fixed' alternatives for classes extending 'cat_categorical'.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
as_categorical<-categorical




#' @method format cat_categorical
#' @S3method format cat_categorical
#' @export
format.cat_categorical<-function(x, ..., cat = FALSE) {
  x<-get_active_values(x)
  single_selection<-all(purrr::map_int(x,length)==1)

  paste0_keepNA<-function(...){
    topaste<-list(...)
    longest_length<-max(purrr::map_dbl(topaste,length))
    topaste_samelength<-lapply(topaste,vec_recycle,longest_length)
    nas <- lapply(topaste_samelength,is.na)
    any_nas <- nas %>% as.data.frame %>% apply(1,function(x){any((x))})

    pasted<-paste0(...)
    pasted[any_nas]<-NA
    pasted

  }

  if(single_selection){return(invisible(paste0_keepNA("'",as.character(unlist(x)),"'")))}
  x<-purrr::map_chr(x,function(x){
    x<-as.character(unclass(x))
    if(cat){
      paste0(
        # number of selected items
        crayon::silver(crayon::italic(paste0(" (",length(x),") "))),
        # concatenated choices
        paste0_keepNA("'",x,"'", collapse = crayon::silver(crayon::italic(" & ")))
      )
    }else{
      paste0(
        # number of selected items
        paste0(" (",length(x),") "),
        # concatenated choices
        paste0_keepNA("'",x,"'", collapse = (" & "))
      )
    }

  })


  invisible(x)
}


#' List all alternative valuse for a categorical vector
#' @param x a categorical vector
#' @param internal logical: If TRUE, show internal alternatives only
#' @return a list with internal and public alternatives as character vectors, or only one of them as a vector if `internal` is set
#' @export
list_alternatives<-function(x,internal = NULL){
  if(is.null(internal)){
    return(list(internal = names(attr(x,'alternatives_internal')), public = names(attr(x,'alternatives'))))

  }
  if(internal){
    return(names(attr(x,'alternatives_internal')))
  }
  return(names(attr(x,'alternatives')))
}

set_active_alternative<-function(x,alternative = character(), internal = FALSE){

  attributes(x)[['active_alternative']]<-alternative
  attributes(x)[['active_alternative_is_internal']]<-internal
  x

}

#' find superficial NAs
#' @param x a <categorical> vectors
#' @details "superficial NA's" appear in categorical vectors where the levels themselves are not NA, but the active alternative has no value for the level
superficial_nas<-function(x){

  active_values <-get_active_values(x)
  level_values  <-get_level_values(x)

  superficial_nas<- is.na(active_values) & !is.na(level_values)
  names(superficial_nas)<-level_values
  return(superficial_nas)
}


#' Set categorical vector to alternative vales
#' @param x categorical vector (see [categorical()])
#' @param alternative the alternative value as a string
#' @return the original vector, but its active values are replaced by the alternative
#' @export
alternate <- function(x,alternative = c(), internal = FALSE){

  # get available alternatives:

  if(length(alternative)==0){
    return(set_active_alternative(x))
  }
  if(internal){
    alt_attribute<- "alternatives_internal"
  }else{
    alt_attribute<- "alternatives"
  }
  alternatives_df<-attr(x,alt_attribute)

  # check requested alternative exists:
  if(!(ncol(alternatives_df)>0)){stop('no alternative attributes available; maybe if you change the `internal` argument?')}
  if(length(alternative)==0){alternative_valid<-TRUE
  }else{
    alternative_valid <- (alternative %in% colnames(alternatives_df)) | (is.numeric(alternative) & alternative <= ncol(alternatives_df))
  }
  if(!alternative_valid){
    stop(paste('can\'t select alternative', alternative, 'from available alternatives:', paste0(names(alternatives_df),collapse = " "),'. Maybe you need to change the `internal` argument?'))
  }

  x<-set_active_alternative(x,
                            alternative = alternative,
                            internal = internal)

  superficial_nas<-superficial_nas(x)
  if(any(superficial_nas)){
    NA_level_names<-unique(names(superficial_nas[superficial_nas==TRUE]))
    warning(paste0(
      "superficial NAs produced (see ?superficial_nas). selected alternative has no values defined for these levels:",
      "'",paste0(NA_level_names,collapse = '\', \''),"'"
    )
    )


  }

  x
}




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
mutate_categorical<-function(.data,...){
  mutation <- rlang::enquos(...)
  .data<-.data %>% dplyr::rowwise %>% dplyr::mutate(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# following the vctrs vignette, adding this wihtout knowing why or what it does:

#' @importFrom methods setOldClass
methods::setOldClass(c("cat_categorical", "vctrs_vctr"))


mr_logical_matrix<-function(x){
  lgl_matrix <- purrr::map(fields(x),field,x=x)   %>% do.call(cbind,.)
}









#' mutate categorical type variables, while treating each choice as logical
#'
#' This is much simpler than it sounds & useful; needs better description & name
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a categorical column. Each row's value is a vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
categorical_logic<-function(x,...){
  mutation <- rlang::enquos(...)

  .data<-as.matrix(x) %>% as.data.frame(stringsAsFactors = FALSE)
  .data<-.data %>% (dplyr::rowwise) %>% dplyr::transmute(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  unlist(.data) %>% unname
}



as.matrix.cat_categorical<-function(x){
  unique_levels<-levels(x)
  logical_matrix <-purrr::map(active(x),function(values){

    1:length(unique_levels) %in% match(values, unique_levels)

  }) %>% do.call(rbind,.)
  colnames(logical_matrix)<-levels(x)
  logical_matrix
}





has_multiple_response<-function(x){
  if(!is_categorical(x)){stop('not a categorical vector')}
  count_selected <- apply(mr_logical_matrix(x),1,function(x){sum(x)})
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
