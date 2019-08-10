
#' @method print cat_select_multiple
print.cat_select_multiple<-function(x, ...) {
  if(length(x)==0){
    levels_text<-levels(x)
    if(length(levels_text)==0){
      levels_text<-"(no levels)"
    }else{
      levels_text<-paste('levels:',paste0(levels_text,collapse = ' '))

      }
    cat(crayon::silver(paste0('select_multiple vector of length 0\n',levels_text)))
    return(invisible(x))
  }
  cat(format.cat_select_multiple(x), sep = " ")
  invisible(x)
}


#' @method obj_print_header cat_select_multiple
#' @S3method obj_print_header cat_select_multiple
#' @importFrom vctrs obj_print_header
obj_print_header.cat_select_multiple<-function(x, ...) {
  cat(crayon::silver(paste0("<select multiple vector>\n")))
  invisible(x)
}


#' @method obj_print_footer cat_select_multiple
#' @S3method obj_print_footer cat_select_multiple
#' @importFrom vctrs obj_print_footer
obj_print_footer.cat_select_multiple<-function(x, ...) {
  cat(crayon::silver(paste0("\nlevels:", paste(crayon::black(levels(x)),collapse = "; "))))
  invisible(x)
}


#' @method obj_print_data cat_select_multiple
#' @S3method obj_print_data cat_select_multiple
#' @importFrom vctrs obj_print_data
obj_print_data.cat_select_multiple<-function(x, ...) {
  cat(format.cat_select_multiple(x,cat=TRUE), sep = "\n")
  invisible(x)
}


# for tibbles:

vec_ptype_abbr.cat_select_multiple <- function(x, ...) {
  "s_mult"
}

pillar_shaft.cat_select_multiple<- function(x, ...) {
  out <- format.cat_select_multiple(x,cat= TRUE)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left", na_indent = 5)
}





#' @importFrom pillar type_sum
#' @export
type_sum.cat_select_multiple <- function(x) {
  "s_mult"
}
