
#' create a new ordinal variable
#'
#' @param x a vector of to be used as values for the ordinal vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param ranks a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
new_ordinal<-function(x, levels, ranks, ...){
  x<-categorical(x = x,levels = levels, alternatives_internal = list(ranks=ranks))
  class(x)<-c('cat_ordinal', class(x))
  x
}



#' create a new ordinal variable
#'
#' @param x a vector of to be used as values for the ordinal vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param rank a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{categorical_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
ordinal <- function(x, levels = unique(unlist(x)), ranks = 1:length(levels), ...) {
  assertthat::assert_that(is.numeric(rank))

  # all is goood, let's go make a new categorical wohay:
  x <- new_ordinal(x = x,
                   levels = levels,
                   ranks=ranks,
                   ...
  )
}


#' create a new ordinal variable
#'
#' @param x a vector of to be used as values for the ordinal vector. These should be characters for most use cases (but can be other types)
#' @param levels vector of of possible values for x; similar to factor levels
#' @param rank a vector of numeric ranks corresponding to each level.
#' @param ... named vectors with alternative values corresponding to 'levels'. Must each have the same length as levels. Can be accessed with \code{ordinal_alternative}. These "external" alternatives are open to user defined alternatives, for example labels in multiple languages.
#' @export
as_ordinal<-ordinal




#' @method format cat_ordinal
#' @S3method format cat_ordinal
#' @export
format.cat_ordinal<-function(x, ..., cat = FALSE) {
  single_selection<-all(purrr::map_int(x,length)==1)

  ranks<-categorical_alternative(x, 'ranks', internal = TRUE)
  x_text<-purrr::map2_chr(x,ranks,function(x,rank){
    x<-as.character(x)
    if(cat){
      paste0(
        # number of selected items
        paste0(crayon::silver(crayon::italic(paste0("#",rank,"-"))),unlist(unclass(x))),
        # concatenated choices
       collapse = crayon::silver(crayon::italic(" & "))
      )
    }else{
      paste0(
        # number of selected items
        paste0(paste0("#",rank,"-",x),
               collapse = (" & ")       # multiple choices concatenated with "&"

        )
      )
    }

  })


  invisible(unlist(x_text))
}

vec_proxy_compare.cat_ordinal<-function(x,...){
  (categorical_alternative(x,'ranks',T) %>%
             unlist %>% as.numeric)}




# categorical_alternative<-function(x,alternative = 1, internal = FALSE){
#
#   if(internal){
#     alt_attribute<- "alternatives_internal"
#   }else{
#     alt_attribute<- "alternatives"
#   }
#   alternatives_df<-attr(x,alt_attribute)
#
#   # check requested alternative exists:
#   if(!(ncol(alternatives_df)>0)){stop('no alternative attributes available; maybe if you change the `internal` argument?')}
#
#
#   alternative_valid <- (alternative %in% colnames(alternatives_df)) | (is.numeric(alternative) & alternative <= ncol(alternatives_df))
#   if(!alternative_valid){
#     stop(paste('can\'t select alternative', alternative, 'from available alternatives:', paste0(names(alternatives_df),collapse = " "),'. Maybe you need to change the `internal` argument?'))
#   }
#
#   levels<-attr(x,'levels')
#
#
#   alternative_indices<-purrr::map(x, match, table = levels)
#   alternative_values<- purrr::map(alternative_indices,function(x){alternatives_df[x, alternative] %>% unname %>% unlist})
#   categorical(alternative_values,levels = unique(unlist(alternative_values)),alternatives_internal = list(original = x))
# }





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

#' Mutate ordinal type variables in a data frame
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a ordinal column. Each row's value is a vector with the selected responses.
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
methods::setOldClass(c("cat_ordinal", "vctrs_vctr"))
