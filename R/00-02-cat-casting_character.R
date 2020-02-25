
#' the functions in this file determine how categorical vectors are converted to other vector types
#' this follows the vctrs package casting system (see ?browseVignettes("vctrs") for details)


#' common type: cat_categorical & character
#'
#' @method vec_ptype2.cat_categorical character
#' @export
vec_ptype2.cat_categorical.character<-function(x,y,...){
  assert_that_not_alternated(x)
  y<-categorical(y)
  vec_ptype2.cat_categorical.cat_categorical(x,y)
}

#' common type: character & cat_categorical
#'
#' @method vec_ptype2.character cat_categorical
#' @export
vec_ptype2.character.cat_categorical<-function(x,y,...){

  x<-categorical(x)
  assert_that_not_alternated(y)
  # passing y first so that new levels from characters are always added in the end;
  # if we don't do that this fails: 'a' ==categorical(c('b','c'))
  # because casting the two sides into each other doesn't return identical types (different order of levels)
  vec_ptype2.cat_categorical.cat_categorical(x,y)
}


#' cast cat_categorical to character
#'
#' @method vec_cast.character cat_categorical
#' @export
vec_cast.character.cat_categorical <- function(x,to,...) {

  as.character(get_active_values(x))

}

#' cast character to cat_categorical
#'
#' @method vec_cast.cat_categorical character
#' @export
vec_cast.cat_categorical.character <- function(x,to,...) {
  y<-to

  x_values<- unique(x)

  y_values<-get_active_alternative_level_values(y)

  y_levels<-levels(y)
  # set x levels to y levels where active values matched:
  x_levels <- y_levels[match(x,y_values)]
  # use x value as new levels where no match found:
  x_levels[is.na(x_levels)]<-x_values[is.na(x_levels)]

  x_categorical<-categorical(x_levels)

  vec_cast.cat_categorical.cat_categorical(x_categorical,y)

}


# NOTE on casting categorical vectors when they are set to alternative values:
# casting vectors in alternated states is a non-trivial problem
# it requires a LOT of decisions about how casting behaves and it will take a lot of thought to make this work
# without creating behaviour that is quietly different from what the user will expect.
# to manage scope for now, categorical vectors must be in unalternated state in order to be casted.

assert_that_not_alternated<-function(x){
  if(is_alternated(x)){stop("can't do this on alternated categorical vectors use unalternate or convert to other type first!")}
}

