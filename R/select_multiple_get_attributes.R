#' @method levels cat_select_multiple
#' @S3method levels cat_select_multiple
levels.cat_select_multiple<-function(x){
  attr(x,"levels")
}


#' @method levels cat_select_multiple
#' @S3method levels cat_select_multiple
labels.cat_select_multiple<-function(x){
  attr(x,"labels")[x]
}


