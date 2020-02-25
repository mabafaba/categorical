#' categorical vectors can 'alternate' between their basic levels and alternative values for each level.
#' the functions in this file all relate to accessing, switching, changing etc. these alternatives



#' Set categorical vector to alternative values
#'
#' @param x categorical vector (see [categorical()])
#' @param alternative the alternative value as a string
#' @param internal logical: set to TRUE if you want to alternate to an internal alternative (useful for vector classes that are based on the categorical class)
#' @return the original vector, but its active values are replaced by the alternative
#' @export
alternate <- function(x,alternative = c(), internal = FALSE){

  # get available alternatives:

  if(length(alternative)==0){
    return(set_active_alternative(x))
  }
  if(internal){
    alt_attribute<- "alternatives_internal"
  }else{
    alt_attribute<- "alternatives"
  }
  alternatives_df<-attr(x,alt_attribute)

  # check requested alternative exists:
  if(!(ncol(alternatives_df)>0)){stop('no alternative attributes available; maybe if you change the `internal` argument?')}
  if(length(alternative)==0){alternative_valid<-TRUE
  }else{
    alternative_valid <- (alternative %in% colnames(alternatives_df)) | (is.numeric(alternative) & alternative <= ncol(alternatives_df))
  }
  if(!alternative_valid){
    stop(paste('can\'t select alternative', alternative, 'from available alternatives:', paste0(names(alternatives_df),collapse = " "),'. Maybe you need to change the `internal` argument?'))
  }

  x<-set_active_alternative(x,
                            alternative = alternative,
                            internal = internal)

  superficial_nas<-superficial_nas(x)
  if(any(superficial_nas)){
    NA_level_names<-unique(names(superficial_nas[superficial_nas==TRUE]))
    warning(paste0(
      "superficial NAs produced (see ?superficial_nas). selected alternative has no values defined for these levels:",
      "'",paste0(NA_level_names,collapse = '\', \''),"'"
    )
    )


  }

  get_active_values(x)
}



#' List all alternative values for a categorical vector
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



#' set a new alternative as active
#' @param x categorical vector
#' @param alternative the name of the alternative to be set as the active one
#' @param internal logical, TRUE if the active alternative is internal
set_active_alternative<-function(x,alternative = character(), internal = FALSE){

  attributes(x)[['active_alternative']]<-alternative
  attributes(x)[['active_alternative_is_internal']]<-internal
  x

}





#' is a categorical vector alternated?
#'
#' @param x a vector of type categorical
#' @return logical, TRUE if the categorical vector is alternated. For details see \code{\link{alternate}}
#' @export
is_alternated<-function(x){
  # if(!is.vector(x)){stop('x must be a vector')}
  if(!is_categorical(x)){return(FALSE)}

  alt_name<-attributes(x)[['active_alternative']]

  if(is.character(alt_name)){
    if(length(alt_name == 1)){
      if(alt_name!=""){
        return(TRUE)
      }
    }
  }
  return(FALSE)

}
