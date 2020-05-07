#' categorical vectors can 'alternate' between their basic levels and alternative values for each level.
#' the functions in this file all relate to accessing, switching, changing etc. these alternatives

#' Set categorical vector to alternative values
#'
#' @param x categorical vector (see [categorical()])
#' @param alternative the alternative value as a string
#' @param internal logical: set to TRUE if you want to alternate to an internal alternative (useful for vector classes that are based on the categorical class)
#' @return the original vector, but its active values are replaced by the alternative
#' @export
alternate<-function(x,alternative = c(), internal = FALSE){
  if(length(alternative)==0){
    return(x)
  }


  alt_level_values <- alternative_levels(x = x, alternative,internal)
  original_levels<-levels(x)
  convert_values_to_alt<-function(vals){

    alt_level_values[
      match(
        vals,
        original_levels
      )
      ]
  }

  if(!any_multiple_selected(x)){
    return(convert_values_to_alt(get_level_values(x)))
  } else{
    return(lapply(get_level_values(x),convert_values_to_alt))
  }





}


#' List all alternative levels for a categorical vector
#'
#' @param x a categorical vector
#' @param internal logical: If TRUE, show internal alternatives only
#' @return a list with internal and public alternatives as character vectors, or only one of them as a vector if `internal` is set
#' @export
list_alternatives<-function(x,internal = NULL){
  if(is.null(internal)){
    return(list(internal = names(attr(x,'alternatives_internal')), public = names(attr(x,'alternatives'))))

  }
  if(internal){
    return(names(attr(x,'alternatives_internal')))
  }
  return(names(attr(x,'alternatives')))
}


