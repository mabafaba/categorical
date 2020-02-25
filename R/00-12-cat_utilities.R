#' ensures that the provided alternative values have as many rows as there are levels
#'
#' @param alternatives a tibble from attributes(some_categorical_vector)$alternatives
#' @param levels character vector with the corresponding levels
enforce_alternative_lengths_match_levels<-function(alternatives,levels){

  if(is.null(alternatives)){return(alternatives)}

  if(is.data.frame(alternatives)){
    if(!nrow(alternatives)==length(levels)){
      stop("all provided alternatives must have exactly one value per categorical level")
    }
    return(alternatives)
  }

  if(is.list(alternatives)){
    # remove empty elements
    alternatives[sapply(alternatives, is.null)] <- NULL
    if(length(alternatives)==0){NULL}
    # stop if not all remaining have the same length as levels
    if(!all(purrr::map_dbl(alternatives,vctrs::vec_size) == length(levels))){stop("all provided alternatives must have exactly one value per categorical level")}
    return(alternatives)
  }
  # for data frames, check nrow equals length levels:



  stop("alternatives must be a list or a data.frame")
}
