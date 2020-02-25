
# helper functions for casting categorical vectors ------------------------


#' combine the levels attribute of two categorical vectors
#' @param x first categorical vector
#' @param y second categorical vector
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


#' combine values of two categorical vectors
#' @param x,y categorical vectors
join_values<-function(x,y){
  c(get_level_values(x),get_level_values(y))
}


join_active_alternative<-function(x,y){
  alt_x<-get_active_alternative_name(x)
  alt_y<-get_active_alternative_name(y)
  if(length(alt_x)==0){
    cat_to_use<-y
  }else if(length(alt_y)==0){
    cat_to_use<-x
    }else{
      cat_to_use<-x
      }


  active<-get_active_alternative_name(cat_to_use)
  attributes(active)$is_internal<-get_active_alternative_is_internal(cat_to_use)
  if(length(attributes(active)$is_internal)==0){
    attributes(active)$is_internal<-FALSE
  }
  return(active)
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

  common_level_type<-vec_ptype2(levels(x),levels(y))

  alternatives_x[[unused_colname]]<-vec_cast(levels(x),common_level_type)
  alternatives_y[[unused_colname]]<-vec_cast(levels(y),common_level_type)

  # reorder levels / alternatives to make sure they match where they match

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


#'
#'
repair_joint_alternatives<-function(x,y,joined_alternatives){

  # prevent R CMD CHECK warning 'no visible binding'
  `____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__`<-NULL

  alternatives_no_duplicate_levels <-

    dplyr::summarise_all(
      dplyr::group_by(joined_alternatives,
                      `____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__`
      )
      ,dplyr::first)

  alternatives_no_duplicate_levels <- alternatives_no_duplicate_levels[match(join_levels(x,y),
                                                                             alternatives_no_duplicate_levels[["____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__"]]
  ),]
  if(nrow(alternatives_no_duplicate_levels)!=nrow(joined_alternatives)){
    warning("discarded duplicate alternative definitions for same levels")
  }
  alternatives_no_duplicate_levels
}


# vec_ptype2.cat_categorical.double <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.integer <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.list <- function(x, y, ...) new_categorical()
# vec_ptype2.cat_categorical.cat_select_one <- function(x, y, ...) new_categorical()

alternative_conflicts<-function(alt_x,alt_y,levels_x,levels_y){
  # remove levels that do not appear in both x and y

  # ... from alternatives
  alt_x<- alt_x[levels_x %in% levels_y,]
  alt_y<- alt_y[levels_y %in% levels_x,]

  # ... from levels:
  levels_x <- levels_x[levels_x %in% levels_y] # keep x that are in y
  levels_y <- levels_y[levels_y %in% levels_x] # keep y that are in x

  # make sure level order is same
  order_levels_x <- order(levels_x)
  order_levels_y <-order(levels_y)

  levels_x <- levels_x[order_levels_x]
  levels_y <- levels_y[order_levels_y]

  if(!all(levels_x==levels_y)){stop("internal bug: x and y should have identical levels at this point")}

  alt_x <-alt_x[order_levels_x,]
  alt_y <-alt_y[order_levels_y,]

  # remove alternative columns that do not appear in both
  common_alternatives<-names(alt_x)[names(alt_x) %in% names(alt_y)]

  # find the missmatches for each of the common alternatives
  alternative_missmatches<-purrr::map(common_alternatives,function(var){


    values<-data.frame(x_alt_values = alt_x[, var],
                       y_alt_values = alt_y[, var],
                       stringsAsFactors = FALSE)

    alternative_values_match <- is_same(values[,1],values[,2])
    levels_x[!alternative_values_match]
  })




  names(alternative_missmatches)<-common_alternatives


  alternative_missmatches<-purrr::map2(alternative_missmatches,names(alternative_missmatches),function(x,var){
    if(length(x)>0){return(data.frame(var = var,missmatch = TRUE,levels_text = paste0(x,collapse = c(', ')),stringsAsFactors = FALSE))}
    return(data.frame(var = var, missmatch = FALSE, levels_text = "",stringsAsFactors = FALSE))
  })

  alternative_missmatches <-  do.call(rbind,alternative_missmatches)

  # prevent 'no visible binding' R CMD CHECK warning:

  missmatch<-NULL

  if(any(alternative_missmatches$missmatch)){
    alternative_missmatches<- dplyr::filter(alternative_missmatches, missmatch)
    unmatchable_alternative_variables<-alternative_missmatches$var
    # message_text<- paste0('Joining categorical vectors: Alternatives with the same name have different values assigned to the same levels and will be renamed; affected:\n',
    #                        paste0(unmatchable_alternative_variables,": ", alternative_missmatches$levels_text[alternative_missmatches$missmatch],collapse = "\n"))
    # warning(error_message)
  }else{
    unmatchable_alternative_variables<-character(0)
  }

  as.character(unmatchable_alternative_variables)

}

