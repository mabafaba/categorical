
#' create a select multiple variable from a character vector, list or binary matrix
#' @param x a character vector with concatenated select_multiple choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' @export
select_multiple <- function(x = character(), choices = NULL, labels = NULL, sep = " ") {
  # if(class(x)=='matrix' & typeof(x)=='logical'){
  # # return(gather_select_multiple(x))
  # }
  if(!is.list(x)){x<-vctrs::vec_cast(x,character())}
  if(is.list(x)){x<-purrr::map(x,vctrs::vec_cast, character())}
  new_select_multiple(x, choices, labels = labels, sep)
}

#' create a new select_multiple variable
#'
#' @param x a character vector with concatenated select_multiple choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
new_select_multiple<-function(x = character(), choices = NULL, labels = NULL, sep = " "){

UseMethod("new_select_multiple")

}


new_select_multiple.character<-function(x = character(), choices = NULL, labels = NULL, sep = " "){
  x<-as.character(x)
  # prepare factor levels
  x_split<-strsplit(x,split = sep)
  new_select_multiple.list(x = x_split, choices = choices, labels  = labels, sep = sep)
}

new_select_multiple.list<-function(x = character(), choices = NULL, labels = NULL, sep = " "){
  x_split<-x

  choices<-as.character(choices)

  # get choices from supplied choices, all factor levels and values:
  choices<- c(choices,levels(unlist(x)),as.character(unlist(x))) %>% unique

  # convert to list of factor vectors
  x_split<-lapply(x_split,function(x){
    factor(x,levels = choices)
  })
  attributes(x_split)$choices<-choices
  # class(x_split)<-c('select_multiple')
  vctrs::new_vctr(x_split, class = "cat_select_multiple", labels = labels, levels = choices)
}

new_select_multiple.cat_select_multiple<-function(x, choices = NULL, labels = NULL, sep = " "){
  choices<-c(levels(x),choices)
  labels<-c(attr(x,'labels'),labels) %>% unique
  new_select_multiple.list(x,choices = choices,labels = labels)
}


#' convert to select_multiple variable
#'
#' @param x a character vector with concatenated select_multiple choices (for example 'c("choice_A choices_B", "choice_C")`)
#' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' @export
as_select_multiple<-select_multiple




#' @method format cat_select_multiple
#' @S3method format cat_select_multiple
#' @export
format.cat_select_multiple<-function(x, ..., cat = FALSE) {

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

# basic type functions

#' check if vector is of class cat_select_multiple
#' @param x a vector
#' @return TRUE if it is
#' @export
is_select_multiple<-function(x){
  inherits(x,'cat_select_multiple')
}

# basic type functions

#' check if vector is of class cat_select_multiple
#' @param x a vector
#' @return TRUE if it is
#' @export
is.select_multiple<-is_select_multiple




#' @export
as.logical.cat_select_multiple<-function(x, ...){
  categorical::spread_select_multiple(x)
}



#' Mutate select_multiple type variables in a data frame
#' @param .data a data.frame or tibble
#' @param ... arguments passed to dplyr::mutate
#' @details operates rowwise (see ?dplyr::rowwise) on a select_multiple column. Each row's value is a factor vector with the selected responses.
#' @return see ?dplyr::mutate
#' @export
mutate_select_multiple<-function(.data,...){
  mutation <- enquos(...)
  .data<-.data %>% dplyr::rowwise %>% dplyr::mutate(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# fct_collapse.cat_select_multiple<-function(x,...){
#   as_select_multiple(lapply(x,fct_collapse,...))
# }
