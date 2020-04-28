#' @method format cat_categorical
#' @S3method format cat_categorical
#' @export
format.cat_categorical<-function(x, ..., cat = FALSE) {
  x<-get_level_values(x)
  single_selection<-all(purrr::map_int(x,length)==1)

  paste0_keepNA<-function(...,collapse = NULL){
    topaste<-list(...)
    longest_length<-max(purrr::map_dbl(topaste,length))
    topaste_samelength<-lapply(topaste,vctrs::vec_recycle,longest_length)
    nas <- lapply(topaste_samelength,is.na)
    any_nas <- nas %>% as.data.frame %>% apply(1,function(x){any((x))}) %>% any

    pasted<-paste0(...,collapse = collapse)
    pasted[any_nas]<-NA
    pasted

  }

  if(single_selection){return(invisible(paste0_keepNA("'",as.character(unlist(x)),"'")))}
  x<-purrr::map_chr(x,function(x){
    x<-as.character(unclass(x))
    if(length(x)==0){
      if(!cat){return(invisible(paste0("(0)")))}else{

        return(invisible(crayon::silver(crayon::italic(paste0(" (",length(x),") ")))))
      }


    }
    multiple_separator<-", "

    if(cat){
      x <- paste0(
        # number of selected items
        ifelse(!any(is.na(x)),
               crayon::silver(crayon::italic(paste0(" (",length(x),") "))),""),
        # concatenated choices
        paste0_keepNA("'",x,"'", collapse = crayon::silver(crayon::italic(multiple_separator)))
      )
    }else{
      x <- paste0(
        # number of selected items
        ifelse(!any(is.na(x)),paste0(" (",length(x),") "),""),
        # concatenated choices
        paste0_keepNA("'",x,"'", collapse = (multiple_separator))
      )
    }

  })


  invisible(x)
}




#' @method print cat_categorical
#' @S3method print cat_categorical
print.cat_categorical<-function(x, ...) {

    levels_text<-levels(x)
    if(length(levels_text)==0){
      levels_text<-"(no levels)"
    }else{
      levels_text<-paste('levels:',paste0(levels_text,collapse = ' '),"\n")

    }
    if(length(x)==0){cat(crayon::silver(paste0('categorical vector of length 0\n',levels_text)))
    return(invisible(x))
    }

  cat(format.cat_categorical(x,cat = FALSE), sep = " ") # using cat = FALSE to turn off colors to speed up printing
  cat(paste0('\n',crayon::silver(levels_text)))
  invisible(x)
}


#' @method obj_print_header cat_categorical
#' @S3method obj_print_header cat_categorical
#' @importFrom vctrs obj_print_header
obj_print_header.cat_categorical<-function(x, ...) {
  cat(crayon::silver(paste0("<categorical vector>\n")))
  invisible(x)
}


#' @method obj_print_footer cat_categorical
#' @S3method obj_print_footer cat_categorical
#' @importFrom vctrs obj_print_footer
obj_print_footer.cat_categorical<-function(x, ...) {
  cat(crayon::silver(paste0("\nlevels:", paste(crayon::black(levels(x)),collapse = "; "))))
  invisible(x)
}


#' @method obj_print_data cat_categorical
#' @S3method obj_print_data cat_categorical
#' @importFrom vctrs obj_print_data
obj_print_data.cat_categorical<-function(x, ...) {
  cat(format.cat_categorical(x,cat=TRUE), sep = "\n")
  invisible(x)
}


# for tibbles:

vec_ptype_abbr.cat_categorical <- function(x, ...) {
  "ctgrcl"
}

pillar_shaft.cat_categorical<- function(x, ...) {
  out <- format.cat_categorical(x,cat= TRUE)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left", na_indent = 5)
}





#' @importFrom pillar type_sum
#' @export
type_sum.cat_categorical <- function(x) {
  "ctgrcl"
}
