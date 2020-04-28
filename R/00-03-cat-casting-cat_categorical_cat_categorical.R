
#' common type of cat_categorical and cat_categorical
#'
#' @inheritParams vec_ptype2.cat_categorical
#' @method vec_ptype2.cat_categorical cat_categorical
#' @export
vec_ptype2.cat_categorical.cat_categorical<-function(x,y,...){

  x<-vec_ptype(x)
  y<-vec_ptype(y)
  out_levels<-join_levels(x,y)
  out_values<-list()
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)


  out_class <- unique(c(class(x),class(y)))
  out_class <- out_class[! out_class %in% class(categorical())]



  new_categorical(matrix(logical(),nrow = 0,ncol = length(out_levels)),levels = out_levels,
                  alternatives_internal = out_alternatives_internal,
                  alternatives = out_alternatives,
                  class = out_class
  )
}

#' cast cat_categorical to cat_categorical
#' @inheritParams vec_cast.cat_categorical
#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.cat_categorical.cat_categorical <- function(x,to,...) {

  assert_that_not_alternated(x)
  assert_that_not_alternated(to)

  y<-to
  out_levels<-join_levels(x,y)
  out_values<-join_values(x,vec_ptype(y))
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)
  level_order <- order(out_levels)

  if(!all(out_levels == out_levels[level_order])){
    # warning("reordered categorical vector levels!")
  }

  out_levels<-out_levels[level_order]
  out_alternatives_internal <- out_alternatives_internal[ level_order, ]
  out_alternatives <- out_alternatives[ level_order, ]



  categorical(x = out_values,
              levels = out_levels,
              alternatives_internal = out_alternatives_internal,
              alternatives = out_alternatives,
              class = class(to)[!(class(to)%in%class(categorical()))]
  )

}



