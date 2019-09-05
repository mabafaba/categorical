
#' @method print cat_categorical
#' @S3method print cat_categorical
print.cat_categorical<-function(x, ...) {
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
  cat(format.cat_categorical(x), sep = " ")
  invisible(x)
}


#' @method obj_print_header cat_categorical
#' @S3method obj_print_header cat_categorical
#' @importFrom vctrs obj_print_header
obj_print_header.cat_categorical<-function(x, ...) {
  cat(crayon::silver(paste0("<ordinal vector>\n")))
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
