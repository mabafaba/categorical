#' @method vec_ptype2.cat_categorical character
#' @export
vec_ptype2.cat_categorical.character<-function(x,y,...){

  out_multiple_selection<-has_multiple_response(x)
  y<-categorical(y)
  vec_ptype2.cat_categorical.cat_categorical(x,y)
}


#' @method vec_ptype2.cat_categorical character
#' @export
vec_ptype2.character.cat_categorical<-function(x,y,...){

  x<-categorical(x)
  vec_ptype2.cat_categorical.cat_categorical(x,y)
}



#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.character.cat_categorical <- function(x,to,...) {

  as.character(get_active_values(x))

}


#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.cat_categorical.character <- function(x,to,...) {
  y<-to

  x_values<- unique(x)
  y_values<-get_active_alternative_level_values(y)

  y_levels<-levels(y)
  # set x levels to y levels where active values matched:
  x_levels <- y_levels[match(x_values,y_values)]
  # use x value as new levels where no match found:
  x_levels[is.na(x_levels)]<-x_values[is.na(x_levels)]

  x_categorical<-categorical(x_levels)

  vec_cast.cat_categorical.cat_categorical(x_categorical,y)

}



#' #' @method vec_cast.cat_categorical cat_categorical
#' #' @export
#' vec_cast.character.cat_categorical <- function(x,to,...) {
#'   y<-to
#'   out_levels<-join_levels(x,y)
#'   out_values<-join_values(x,y)
#'   out_alternatives<-join_alternatives(x,y,FALSE)
#'   out_alternatives_internal<-join_alternatives(x,y,TRUE)
#'   out_multiple_selection<-has_multiple_response(x) | has_multiple_response(y)
#'   new_categorical(x = out_values,
#'                   levels = out_levels,
#'                   alternatives_internal = out_alternatives_internal,
#'                   alternatives = out_alternatives,
#'                   multiple_selection = out_multiple_selection)
#'
#' }
