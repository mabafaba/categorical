


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



#' create a select multiple variable from a character vector, list or binary matrix
#' @param x a character vector with concatenated categorical choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' @export
categorical <- function(x, levels = unique(unlist(x)), alternatives_internal = tibble::tibble(.rows = length(levels)), ...) {

  assertthat::assert_that(all(unique(unlist(x,use.names = FALSE)) %in% levels))

  # values should always be a list in the end
  if(!is.list(x)){
    x<-as.list(x)
  }


  # alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  if(is.null(alternatives)){
    alternatives_internal<-tibble::tibble(.rows = length(levels))
  }

  alternatives_internal<-tibble::as_tibble(alternatives_internal, .rows = length(levels))

  # public alternatives should always be(come) a tibble, and should always have as many rows as levels exist:
  public_alternatives<-list(...)
  public_alternatives<-tibble::as_tibble(public_alternatives,.rows = length(levels))

  # all is goood, let's go make a new categorical wohay:
  new_categorical(x = x,
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = public_alternatives)
}



#' create a new categorical variable
#'
#' @param x a character vector with concatenated categorical choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
new_categorical <- function(x, levels,
                            alternatives_internal = tibble::tibble(.rows = length(levels)),
                            alternatives = tibble::tibble(.rows = length(levels))) {

  vctrs::new_vctr(x, class = "cat_categorical",
                  levels = levels,
                  alternatives_internal = alternatives_internal,
                  alternatives = alternatives)

}


#' convert to categorical variable
#'
#' @param x a character vector with concatenated categorical choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' @export
as_categorical<-categorical




#' @method format cat_categorical
#' @S3method format cat_categorical
#' @export
format.cat_categorical<-function(x, ..., cat = FALSE) {

  x<-purrr::map_chr(x,function(x){
    x<-as.character(x)
    if(cat){
      paste0(
        # number of selected items
        crayon::silver(crayon::italic(paste0(" (",length(x),") "))),
        # concatenated choices
        paste0(x, collapse = crayon::silver(crayon::italic(" & ")))
      )
    }else{
      paste0(
        # number of selected items
        paste0(" (",length(x),") "),
        # concatenated choices
        paste0(x, collapse = (" & "))
      )
    }

  })


  x
}

categorical_alternative<-function(x,alternative = 1, internal = FALSE){

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


 alternative_indices<-purrr::map(x, match, table = levels)
 alternative_values<- purrr::map(alternative_indices,function(x){alternatives_df[x, alternative] %>% unname %>% unlist})
 categorical(alternative_values,levels = unique(unlist(alternative_values)))
}





# basic type functions

#' check if vector is of class cat_categorical
#' @param x a vector
#' @return TRUE if it is
#' @export
is_categorical<-function(x){
  inherits(x,'cat_categorical')
}

# basic type functions

#' check if vector is of class cat_categorical
#' @param x a vector
#' @return TRUE if it is
#' @export
is.categorical<-is_categorical

#' Mutate categorical type variables in a data frame
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a categorical column. Each row's value is a factor vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
mutate_categorical<-function(.data,...){
  mutation <- enquos(...)
  .data<-.data %>% dplyr::rowwise %>% dplyr::mutate(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# following the vctrs vignette, adding this wihtout knowing why or what it does:

#' @importFrom methods setOldClass
methods::setOldClass(c("cat_categorical", "vctrs_vctr"))
