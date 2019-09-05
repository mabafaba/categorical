


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
#' @export
categorical <- function(x = character(), levels = unique(unlist(x)), alternatives_internal = tibble::tibble(.rows = length(levels)), ...) {

  assertthat::assert_that(all(unique(unlist(x,use.names = FALSE)) %in% levels))

  public_alternatives<-list(...)

  # make sure alternatives have same length too:

  alternative_lengths<-purrr::map_int(alternatives_internal,length)
  bad_length<-(alternative_lengths != length(levels))
  if(any(bad_length)){
    stop(paste('internal alternative(s) with wrong length (must be same length as levels):',
               paste(names(public_alternatives)[bad_length],collapse = " ")))
  }

  alternative_lengths<-purrr::map_int(public_alternatives,length)
  bad_length<-(alternative_lengths != length(levels))
  if(any(bad_length)){
    stop(paste('alternative(s) with wrong length (must be same length as levels):',
               paste(names(public_alternatives)[bad_length],collapse = " ")))
  }



  # values should always be a list in the end
  if(!is.list(x)){
    x<-as.list(x)
  }


  # alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  if(is.null(alternatives_internal)){
    alternatives_internal<-tibble::tibble(.rows = length(levels))
  }

  alternatives_internal<-tibble::as_tibble(alternatives_internal, .rows = length(levels))

  # public alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  public_alternatives<-tibble::as_tibble(public_alternatives,.rows = length(levels))



  # all is goood, let's go make a new categorical wohay:
  new_categorical(x = x,
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = public_alternatives)
}



#' @method levels cat_categorical
#' @S3method levels cat_categorical
levels.cat_categorical<-function(x){
  attr(x,'levels')
}


#' create a new categorical variable
#'
#' @param x a vector or list to be used as values for the categorical vector
#' @param levels list of possible values for x; similar to factor levels. Defaults to the unique values in x
#' @param alternatives_internal a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. "internal" alternatives are used to store 'fixed' alternatives for classes extending 'cat_categorical'.
#' @param alternatives a named list of vectors with alternative values corresponding to 'levels'. Must have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
new_categorical <- function(x = character(), levels,
                            alternatives_internal = tibble::tibble(.rows = length(levels)),
                            alternatives = tibble::tibble(.rows = length(levels))) {

  vctrs::new_rcrd(fields = list(active_value = x, level_value = x), class = "cat_categorical",
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = alternatives)

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
  x<-vctrs::field(x,'active_value')
  single_selection<-all(purrr::map_int(x,length)==1)
  if(single_selection){return(invisible(paste0("'",as.character(unlist(x)),"'")))}
  x<-purrr::map_chr(x,function(x){
    x<-as.character(unclass(x))
    if(cat){
      paste0(
        # number of selected items
        crayon::silver(crayon::italic(paste0(" (",length(x),") "))),
        # concatenated choices
        paste0("'",x,"'", collapse = crayon::silver(crayon::italic(" & ")))
      )
    }else{
      paste0(
        # number of selected items
        paste0(" (",length(x),") "),
        # concatenated choices
        paste0("'",x,"'", collapse = (" & "))
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

#' Set categorical vector to alternative vales
#' @param x categorical vector (see \link{\code{categorical}})
#' @param alternative the alternative value as a string
#' @return the original vector, but its active values are replaced by the alternative
#' @export
alternate<-function(x,alternative = NULL, internal = FALSE){

  if(is.null(alternative)){

    # if(!has_multiple_response(x)){
    #   vctrs::field(x,'active_value')<-
    #     unlist(vctrs::field(x,'level_value'))
    #
    # }
    vctrs::field(x,'active_value')<-
        unlist(vctrs::field(x,'level_value'))
    return(x)
  }

  if(internal){
    alt_attribute<- "alternatives_internal"
  }else{
    alt_attribute<- "alternatives"
  }
  alternatives_df<-attr(x,alt_attribute)

  # check requested alternative exists:
  if(!(ncol(alternatives_df)>0)){stop('no alternative attributes available; maybe if you change the `internal` argument?')}


  alternative_valid <- (alternative %in% colnames(alternatives_df)) | (is.numeric(alternative) & alternative <= ncol(alternatives_df))
  if(!alternative_valid){
      stop(paste('can\'t select alternative', alternative, 'from available alternatives:', paste0(names(alternatives_df),collapse = " "),'. Maybe you need to change the `internal` argument?'))
  }

  levels<-attr(x,'levels')


 alternative_indices<-purrr::map(vctrs::field(x,'level_value'), match, table = levels)
 alternative_values<- purrr::map(alternative_indices,function(x){alternatives_df[x, alternative] %>% unname %>% unlist})
 vctrs::field(x,'active_value')<-alternative_values
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



level_values<-function(x){
  vctrs::field(x,'level_value')
}

active_values<-function(x){
  vctrs::field(x,'level_value')
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
  if(!is.list(x)){return(FALSE)}
  !all(purrr::map_dbl(field(x,'active_value'),length)==1)
}

