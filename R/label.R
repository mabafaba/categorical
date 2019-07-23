
#' apply labels to categorical data
#'
#' @param x a categorical vector of class categorical_select_multiple or categorical_select_one
#' @return a character vector with the labels of `x`
#' @seealso \code{\link{select_multiple}}
#' @export
label<-function(x){
  UseMethod('label', x)
}

#' labels for select multiple
#' @param x a list of class categorical_select_multiple
#' @return same as `x`, but all values are replaced by values; lookup table for labels no longer part of attributes.
label.cat_select_multiple<-function(x){
  labels<-attributes(x)$labels
  lapply(x, function(y){
    y_labeled<-as.character(y)
    y_labeled[y %in% names(labels)]<-labels[y_labeled[y %in% names(labels)]]
    y_labeled
  }) %>% as_select_multiple(choices = labels)
}
