
c.factor <- function(..., recursive=TRUE) unlist(list(...), recursive=recursive)

#' @export
`+.cat_select_multiple`<-function(e1,e2){
  if(length(which(c(is_select_multiple(e1),is_select_multiple(e2))))!=2){stop("logical operator + for select multiple only works if both arguments are select_multiple")}

  choices<-unique(c(levels(e1),levels(e2)))
  labels<-c(attributes(e1)$labels,attributes(e2)$labels)
  labels<-labels[!duplicated(names(labels))]

  purrr::map2(e1,e2,c) %>% purrr::map(unique) %>% new_select_multiple.list(choices = choices,labels = labels)
}


#' @export
`-.cat_select_multiple`<-function(e1,e2){
  if(length(which(c(is_select_multiple(e1),is_select_multiple(e2))))!=2){stop("logical operator - for select multiple only works if both arguments are select_multiple")}

  choices<-levels(e1)
  labels<-c(attributes(e1)$labels)
  purrr::map2(e1,e2,function(x,y){x[!(as.character(x)%in%as.character(y))]}) %>% new_select_multiple.list(choices = choices,labels = labels)
}





#' @export
`==.cat_select_multiple`<-function(e1,e2){
  stop('== is not applicable for select_multiple data types, because it is not clear if exactly the same choices should be selected, any or all. Use one of the more specific %==any%, %==all%, %==exactly%')

}

#' @export
`!=.cat_select_multiple`<-function(e1,e2){
  stop('!= is not applicable for select_multiple data types, because it is not clear if exactly the same choices should be selected, any or all. Use one of the more specific %!=any%, %!=all%, %!=exactly%')

}

#' Logical operators for select multiple
#' @param x values to check
#' @param y values to check against
#' @export
`%==exactly%`<-function(x,y){
  assertthat::assert_that(is_select_multiple(x))
  assertthat::assert_that(is_select_multiple(y))
  # if(length(x)!=length(y) & length(y)!=1){
    # stop("arguments before and after %==exactly% must both be select_multiple class. You can use `select_multiple(x)` to convert.")
  # }

  purrr::map2_lgl(x,y,function(x_entry,y_entry){
    if(length(x_entry)!=length(y_entry)){return(FALSE)}
    all(sort(as.character(x_entry)) == sort(as.character(y_entry)))
  })
}



