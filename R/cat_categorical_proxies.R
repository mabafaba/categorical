

vec_proxy.cat_categorical<-function(x,...){
  c(list(vals = unlist(unclass(x))),
    attr(x,'alternatives_internal'),
    attr(x,'alternatives')
  ) %>% as.data.frame(stringsAsFactors = FALSE)
}
