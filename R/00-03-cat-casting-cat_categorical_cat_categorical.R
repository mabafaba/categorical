
#' common type of cat_categorical and cat_categorical
#'
#' @method vec_ptype2.cat_categorical cat_categorical
#' @export
vec_ptype2.cat_categorical.cat_categorical<-function(x,y,...){



  # out_multiple_selection<-has_multiple_response(x) | has_multiple_response(y)

  x<-vec_ptype(x)
  y<-vec_ptype(y)
  out_levels<-join_levels(x,y)
  out_values<-list()
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)


  active_alt_x_name<-get_active_alternative_name(x)
  active_alt_y_name<-get_active_alternative_name(y)
  if(length(active_alt_y_name)==0){
    out_active_alternative <- active_alt_x_name
    out_active_alternative_is_internal <-   get_active_alternative_is_internal(x)
  }else{
    out_active_alternative <- active_alt_y_name
    out_active_alternative_is_internal <-   FALSE
  }


  new_categorical(matrix(logical(),nrow = 0,ncol = length(out_levels)),levels = out_levels,
                  alternatives_internal = out_alternatives_internal,
                  alternatives = out_alternatives,
                  active_alternative = out_active_alternative,
                  active_alternative_is_internal = out_active_alternative_is_internal

                  # multiple_selection = out_multiple_selection
  )
}

#' cast cat_categorical to cat_categorical
#'
#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.cat_categorical.cat_categorical <- function(x,to,...) {

  assert_that_not_alternated(x)
  assert_that_not_alternated(to)

  y<-to
  out_levels<-join_levels(x,y)
  out_values<-join_values(x,vec_ptype(y), levels = out_levels)
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)
  level_order <- order(out_levels)

  if(!all(out_levels == out_levels[level_order])){
    # warning("reordered categorical vector levels!")
  }

  out_levels<-out_levels[level_order]
  out_alternatives_internal <- out_alternatives_internal[ level_order, ]
  out_alternatives <- out_alternatives[ level_order, ]

  # out_multiple_selection<-has_multiple_response(x) | has_multiple_response(y)
  active_alt_x_name<-get_active_alternative_name(x)
  active_alt_to_name<-get_active_alternative_name(to)
  if(length(active_alt_to_name)==0){
    out_active_alternative <- active_alt_x_name
    out_active_alternative_is_internal <-   get_active_alternative_is_internal(x)
  }else{
    out_active_alternative <- active_alt_to_name
    out_active_alternative_is_internal <-   FALSE
  }



  categorical(x = out_values,
              levels = out_levels,
              alternatives_internal = out_alternatives_internal,
              alternatives = out_alternatives,
              active_alternative = out_active_alternative,
              active_alternative_is_internal = out_active_alternative_is_internal,
              class = class(to)[!(class(to)%in%class(categorical()))]
  )

}



