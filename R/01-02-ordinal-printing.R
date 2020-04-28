




#' @method format cat_ordinal
#' @S3method format cat_ordinal
#' @export
format.cat_ordinal<-function(x, ..., cat = FALSE) {
  single_selection<-all(purrr::map_int(x,length)==1)

  ranks<-alternate(x, 'ranks', internal = TRUE) 


  x<-purrr::map(get_level_values(x),as.character)

  silvit<-function(x){
    if(cat){return(crayon::silver(crayon::italic(x)))}
    return(x)
  }
  format_record_one<-function(x,rank){
    paste0(silvit(paste0("#",rank,"-")),x)
  }


  format_record_multiple<-function(x,rank){
    x<-x[order(rank)]
    rank<-rank[order(rank)]
    paste0(silvit("("),
           paste0(purrr::map2_chr(x,rank,format_record_one),collapse = silvit(" & ")),
           silvit(")")
           )
  }


format_record<-function(x,ranks){
  if(length(x)<=1){
    return(format_record_one(x,ranks))
  }
  format_record_multiple(x,ranks)

}
# this map is for multiple selection
x_text<-purrr::map2_chr(x,ranks,format_record)


invisible(unlist(x_text))
}

#' @method print cat_ordinal
#' @S3method print cat_ordinal
print.cat_ordinal<-function(x, ...) {
  if(length(x)==0){
    levels_text<-levels(x)
    if(length(levels_text)==0){
      levels_text<-"(no levels)"
    }else{
      levels_text<-paste('levels:',paste0(levels_text,collapse = ' '))

    }
    cat(crayon::silver(paste0('ordinal vector of length 0\n',levels_text)))
    return(invisible(x))
  }
  cat(format.cat_ordinal(x), sep = " ")
  invisible(x)
}


#' @method obj_print_header cat_ordinal
#' @S3method obj_print_header cat_ordinal
#' @importFrom vctrs obj_print_header
obj_print_header.cat_ordinal<-function(x, ...) {
  cat(crayon::silver(paste0("<ordinal vector>\n")))
  invisible(x)
}


#' @method obj_print_footer cat_ordinal
#' @S3method obj_print_footer cat_ordinal
#' @importFrom vctrs obj_print_footer
obj_print_footer.cat_ordinal<-function(x, ...) {
  cat(crayon::silver(paste0("\nlevels:", paste(crayon::black(levels(x)),collapse = " "))))
  invisible(x)
}


#' @method obj_print_data cat_ordinal
#' @S3method obj_print_data cat_ordinal
#' @importFrom vctrs obj_print_data
obj_print_data.cat_ordinal<-function(x, ...) {
  cat(format.cat_ordinal(x,cat=TRUE), sep = "\n")
  invisible(x)
}


# for tibbles:

vec_ptype_abbr.cat_ordinal <- function(x, ...) {
  "ord"
}

pillar_shaft.cat_ordinal<- function(x, ...) {
  out <- format.cat_ordinal(x,cat= TRUE)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left", na_indent = 5)
}





#' @importFrom pillar type_sum
#' @export
type_sum.cat_ordinal <- function(x) {
  "ord"
}
