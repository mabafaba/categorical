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


#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.cat_categorical.cat_categorical <- function(x,to,...) {
  y<-to
  out_levels<-join_levels(x,y)
  out_values<-join_values(x,vec_ptype(y), levels = out_levels)
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)
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


join_levels<-function(x,y){
  # if(typeof(levels(x))!=typeof(levels(y))){warning('converting levels to match data type')}
  if(!is_categorical(x)){stop("x must be a categorical vector")}
  if(!is_categorical(y)){stop("y must be a categorical vector")}
  x<-levels(x)
  y<-levels(y)

  # here levels had been converted to factors; reasons unclear;
  # removed because we shouldn't change the level data type for no clear reason (which causes issues when casting)
  # # if exactly one of the inputs is a factor, convert to character
  # if(sum(is.factor(x),is.factor(y)==1)){
  #   if(is.factor(x)){x<-as.character(x)}
  # if(is.factor(y)){y<-as.character(y)}
  # # }
  levels<-unique(c(x,y))

  levels
}

join_values<-function(x,y,levels){
  c(get_level_values(x),get_level_values(y))
}

join_alternatives<-function(x,y,internal = FALSE){
  levels<-join_levels(x,y)
  if(!internal){
    alternatives_x<-attributes(x)$alternatives
    alternatives_y<-attributes(y)$alternatives
  }else{
    alternatives_x<-attributes(x)$alternatives_internal
    alternatives_y<-attributes(y)$alternatives_internal

  }

  common_level_type<-vec_ptype(c(levels(x),levels(y)))

  alternatives_x[[unused_colname]]<-vec_cast(levels(x),common_level_type)
  alternatives_y[[unused_colname]]<-vec_cast(levels(y),common_level_type)

  common_alternative_names<-names(alternatives_x)[names(alternatives_x)%in%names(alternatives_y)]
  # don't join alternatives with same name but conflicting values:
  alternatives_with_conflicting_values <- alternative_conflicts(alt_x = alternatives_x,
                                                                alt_y = alternatives_y,
                                                                levels_x = levels(x),
                                                                levels_y = levels(y))

  common_alternative_names<-common_alternative_names[!(common_alternative_names %in% alternatives_with_conflicting_values)]




  # prepare vector used by dplyr::full_join to decide which columns to merge into one:
  names(common_alternative_names)<-common_alternative_names
  joined_alternatives<-suppressMessages(dplyr::full_join(alternatives_x,alternatives_y,by = common_alternative_names,
                                                         keep=TRUE
  ))


  # warn about any colnames that might have changed:
  new_alternative_names<- names(joined_alternatives)[!(names(joined_alternatives) %in% c(names(alternatives_x),names(alternatives_y)))]
  if(length(new_alternative_names)!=0){
    warning(paste0('created new names for alternatives due to conflicting values: ',paste0(new_alternative_names,collapse = ', ')))
  }
  joined_alternatives <- repair_joint_alternatives(x,y,joined_alternatives)


  # remove temporary column that was used to match on levels:
  joined_alternatives<-joined_alternatives[,colnames(joined_alternatives)!=unused_colname]

  joined_alternatives
}

repair_joint_alternatives<-function(x,y,joined_alternatives){



  alternatives_no_duplicate_levels <-
    joined_alternatives %>%
    dplyr::group_by(`____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__`) %>%
    dplyr::summarise_all(dplyr::first)

  # the group_by %>% summarise_all messed up the order, so:
  alternatives_no_duplicate_levels <- alternatives_no_duplicate_levels[match(join_levels(x,y),
                                                                             alternatives_no_duplicate_levels[["____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__"]]
  ),]
  if(nrow(alternatives_no_duplicate_levels)!=nrow(joined_alternatives)){
    warning("discarded duplicate alternative definitions for same levels")
  }
  alternatives_no_duplicate_levels
}


# vec_ptype2.cat_categorical.character <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.double <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.integer <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.list <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.cat_select_one <- function(x, y, ...) new_categorical()

alternative_conflicts<-function(alt_x,alt_y,levels_x,levels_y){

  x_levels_in_y<-levels_x[levels_x %in% levels_y]
  common_alternatives<-names(alt_x)[names(alt_x) %in% names(alt_y)]

  alternative_missmatches<-purrr::map(common_alternatives,function(var){
    values<-data.frame(x_alt_values = alt_x[match(x_levels_in_y,levels_x), var],
                       y_alt_values = alt_y[levels_y %in% x_levels_in_y,var],stringsAsFactors = FALSE)

    alternative_values_match <- is_same(values[,1],values[,2])
    x_levels_in_y[!alternative_values_match]
  })




  names(alternative_missmatches)<-common_alternatives


  alternative_missmatches<-purrr::map2(alternative_missmatches,names(alternative_missmatches),function(x,var){
    if(length(x)>0){return(data.frame(var = var,missmatch = TRUE,levels_text = paste0(x,collapse = c(', ')),stringsAsFactors = FALSE))}
    return(data.frame(var = var, missmatch = FALSE, levels_text = "",stringsAsFactors = FALSE))
  }) %>% do.call(rbind,.)

  if(any(alternative_missmatches$missmatch)){
    alternative_missmatches<-alternative_missmatches %>% dplyr::filter(missmatch)
    unmatchable_alternative_variables<-alternative_missmatches$var
    # message_text<- paste0('Joining categorical vectors: Alternatives with the same name have different values assigned to the same levels and will be renamed; affected:\n',
    #                        paste0(unmatchable_alternative_variables,": ", alternative_missmatches$levels_text[alternative_missmatches$missmatch],collapse = "\n"))
    # warning(error_message)
  }else{
    unmatchable_alternative_variables<-character(0)
  }

  as.character(unmatchable_alternative_variables)

}

