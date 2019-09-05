#' @method levels cat_ordinal
#' @S3method levels cat_ordinal
levels.cat_categorical<-function(x){
  attr(x,"levels")
}

#' @method levels cat_ordinal
#' @S3method levels cat_ordinal
levels.cat_ordinal <- levels.cat_categorical
