unused_colname<-'____LEVELS_CAT_CATEG_IN_vec_ptype2.cat_categorical.cat_categorical_JOIN_324895683247659__'

#' When the change happens implicitly (e.g in c()) we call it coercion
#' ec_ptype2(x, y) defines possible set of coercions.
#' It returns a prototype if x and y can be safely coerced to the same prototype;
#' otherwise it returns an error.
#' The set of automatic coercions is usually quite small
#' because too many tend to make code harder to reason about
#' and silently propagate mistakes.
#'
#'
#' each combo of our class and other classes that can be coerced toghether has
#' a function that returns just a prototype of the resulting class
#'
#' namespace exports:
#' generic double dispatch boilerplate like this:
#' #' @method vec_cast CLASS
#' #' @export
#' #' @export vec_cast.CLASS
#' vec_cast.vctrs_percent <- function(x, to, ...) {
#' }
#'
#' individual dispatches like this:
#'
#' #' @method vec_cast.CLASS1 CLASS2
#' #' @export


# cat_categorical & cat_categorical:


vec_ptype.cat_categorical<-function(x) x[0]


#' @method vec_ptype2.cat_categorical cat_categorical
#' @export
vec_ptype2.cat_categorical.cat_categorical<-function(x,y,...){



  out_multiple_selection<-has_multiple_response(x) | has_multiple_response(y)

  x<-vec_ptype(x)
  y<-vec_ptype(y)
  out_levels<-join_levels(x,y)
  out_values<-list()
  out_alternatives<-join_alternatives(x,y,FALSE)
  out_alternatives_internal<-join_alternatives(x,y,TRUE)
  new_categorical(x = out_values,
                  levels = out_levels,
                  alternatives_internal = out_alternatives_internal,
                  alternatives = out_alternatives,
                  multiple_selection = out_multiple_selection)
}


#' @method vec_cast.cat_categorical cat_categorical
#' @export
vec_cast.cat_categorical.cat_categorical <- function(x,to,...) {
y<-to
out_levels<-join_levels(x,y)
out_values<-join_values(x,y)
out_alternatives<-join_alternatives(x,y,FALSE)
out_alternatives_internal<-join_alternatives(x,y,TRUE)
out_multiple_selection<-has_multiple_response(x) | has_multiple_response(y)
new_categorical(x = out_values,
                levels = out_levels,
                alternatives_internal = out_alternatives_internal,
                alternatives = out_alternatives,
                multiple_selection = out_multiple_selection)

}


join_levels<-function(x,y){
  # if(typeof(levels(x))!=typeof(levels(y))){warning('converting levels to match data type')}
  levels<-unique(c(levels(x),levels(y)))

  levels
}

join_values<-function(x,y){
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
    dplyr::summarise_all(first)

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
    alternative_missmatches<-alternative_missmatches %>% filter(missmatch)
    unmatchable_alternative_variables<-alternative_missmatches$var
    # message_text<- paste0('Joining categorical vectors: Alternatives with the same name have different values assigned to the same levels and will be renamed; affected:\n',
    #                        paste0(unmatchable_alternative_variables,": ", alternative_missmatches$levels_text[alternative_missmatches$missmatch],collapse = "\n"))
    # warning(error_message)
  }else{
    unmatchable_alternative_variables<-character(0)
  }

  as.character(unmatchable_alternative_variables)

  }





#' BOILERPLATES
#'
#' start with the boilerplate for vec_ptype2()
#' this is just so later we can write vec_ptype2.cat_categorical.OTHERCLASS
#' to define how these two classes should be coerced together
#' @param x vector object
#' @param y vector object
#' @param ... additional arguments
#' @method vec_ptype2 cat_categorical
#' @export
#' @export vec_ptype2.cat_categorical
vec_ptype2.cat_categorical <- function(x, y, ...) UseMethod("vec_ptype2.cat_categorical", y)
vec_ptype2.cat_categorical.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' cast categorical vectors
#' @param x vector
#' @param to prototype to conver to
#' @param ... additional arguments
#' @method vec_cast cat_categorical
#' @export
#' @export vec_cast.cat_categorical
vec_cast.cat_categorical <- function(x, to, ...) UseMethod("vec_cast.cat_categorical")
vec_cast.cat_categorical.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)


