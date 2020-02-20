

# levels and alternatives -------------------------------------------------

get_active_alternative_name<-function(x){
  alt<-attributes(x)[["active_alternative"]]
  if(length(alt)==0){return(c())}
  names(alt)<-ifelse(attributes(x)[["active_alternative_is_internal"]],"internal","public")
  if(is.null(alt)){return(character())}
  alt
}

get_active_alternative_is_internal<-function(x){
  # no active alternative is always considered not internal:
  if(length(get_active_alternative_name(x))==0){return(FALSE)}
  return(attributes(x)$active_alternative_is_internal)
}

#' @method levels cat_ordinal
#' @S3method levels cat_ordinal
levels.cat_categorical<-function(x){
  attr(x,"levels")
}

#' @method levels cat_ordinal
#' @S3method levels cat_ordinal
levels.cat_ordinal <- levels.cat_categorical


get_alternative_level_values<-function(x,alternative,internal = FALSE){

  alts<-alternatives(x,internal = internal)
  if(!(alternative %in% names(alts))){stop(glue::glue('alternative "{alternative}" does not exist (for internal: {internal}); see list_alternatives()'))}
  alts[[alternative]]

}

get_active_alternative_level_values<-function(x){
  internal<-get_active_alternative_is_internal(x)
  active_name<-get_active_alternative_name(x)
  if(length(active_name)==0){return(levels(x))}
  return(get_alternative_level_values(x,active_name,internal))
}


# vector content ----------------------------------------------------------




get_level_values<-function(x){
  values_as_logical_df <- x %>%
    mr_logical_matrix %>%
    t %>%
    as.data.frame

  list_of_selected_values <- values_as_logical_df %>%
    purrr::map(which) %>%
    purrr::map(~ levels(x)[.x]) %>% unname

  x<-restore_lgl_list_NA_in_value_list(list_of_selected_values,x)
  if(all(purrr::map_dbl(x,length)==1)){
    x<-unlist(x)
  }
  return(x)
}



get_active_values<-function(x){

  if(!is_categorical(x)){stop('not a categorical vector')}


  alternative<-get_active_alternative_name(x)
  if(length(alternative)==0){
    return(get_level_values(x))
  }
  is_internal<-names(alternative)=="internal"
  alternative_values<- alternatives(x,internal = is_internal)

  active_values<-x %>%
    mr_logical_matrix %>%
    t %>%
    as.data.frame %>%
    purrr::map(which) %>%
    purrr::map(~ unname(unlist(alternative_values[.x,alternative])))

  active_values <- restore_lgl_list_NA_in_value_list(active_values,x)

  if(all(purrr::map_dbl(active_values,length)==1)){
    x<-unlist(x)
  }

  return(active_values)
}



#' set a list of items to NA where any value in a categorical logical matrix representation is NA
#' @param value_list a list with as many items as there are records in x_categorical
#' @param x_categorical a categorical vector
restore_lgl_list_NA_in_value_list<-function(value_list, x_categorical){

  values_as_logical_df <- x_categorical %>%
    mr_logical_matrix %>%
    t %>%
    as.data.frame

  any_NA_in_lgl_matrix<- purrr::map_lgl(values_as_logical_df,function(x){any(is.na(x))})
  purrr::map2(value_list,any_NA_in_lgl_matrix,function(values,should_be_na){
    if(should_be_na){
      return(NA)
    }
    else{
      return(values)
    }

  })

}
