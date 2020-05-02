



alternatives<-function(x, internal = FALSE){
  if(!is_categorical(x)){stop("not a <categorical> vector")}
  if(!internal){
    return(attributes(x)$alternatives)
  }

  return(attributes(x)$alternatives_internal)
}






alternative_levels<-function(x, alternative, internal = FALSE){

  alts<-alternatives(x,internal = internal)
  if(!(alternative %in% names(alts))){stop(glue::glue('alternative "{alternative}" does not exist (for internal: {internal}); see list_alternatives()'))}
  alts[[alternative]]

}

# get_level_values<-function(x){
#   # PROBLEM: this used to return the active levels or the levels
# return(levels(x))}
# }


# vector content ----------------------------------------------------------



#' get level values
#' @param x categorical vector
#' @details return the raw values of a categorical vector
#' @return a vector with each records level if no multiple selection appears. Otherwise a list, each element containing all levels of one record.
get_level_values<-function(x){
  values_as_logical_df <- x %>% unclass %>% do.call(cbind,.) %>%
    as.matrix %>%
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




#' set a list of items to NA where any value in a categorical logical matrix representation is NA
#'
#' @param value_list a list with as many items as there are records in x_categorical
#' @param x_categorical a categorical vector
restore_lgl_list_NA_in_value_list<-function(value_list, x_categorical){

  values_as_logical_df <- x_categorical %>%
    as.matrix %>%
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
