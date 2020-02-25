#' @export
levels.cat_categorical<-function(x){
  attr(x,"levels")
}


#' table() for categorical vectors
#'
#' @param a categorical vector
#'
#' @export
cl_table<-function(x){
  colSums(as.matrix(x))
}


#' drop levels from categorical vector
#' @export
droplevels.cat_categorical<-function(x, ...){

}


#' remove levels from a categorical vector
#' @param x categorical vector
#' @param choices levels to be removed
#' @export
cat_remove_levels<-function(x, choices){
  if(!is_categorical(x)){stop("x must be a categorical vector")}
  if(!is.character(choices) | !is.vector(choices)){stop('choices must be a character vector')}
  if(!all(choices %in% attributes(x)$levels)){stop("can not drop choices that do not exist")}


    levels_to_keep<-!(levels(x) %in% choices)

    # remove choices vctrs list entries
    original_class<- class(x)
    original_attributes<-attributes(x)
    x<-unclass(x)
    x<-x[levels_to_keep]


    # remove choices from attributes
    new_attributes <- original_attributes
    new_attributes$levels <- original_attributes$levels[levels_to_keep]
    new_attributes$alternatives_internal<-original_attributes$alternatives_internal[levels_to_keep,]
    new_attributes$alternatives <- original_attributes$alternatives[levels_to_keep,]
    new_attributes$names<-original_attributes$names[levels_to_keep]
    # restore object structure
    class(x)<-original_class
    attributes(x)<-new_attributes


    x
  }


