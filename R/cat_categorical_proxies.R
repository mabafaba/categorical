

vec_proxy.cat_categorical<-function(x,...){
  c(list(vals = unlist(unclass(x))),
    attr(x,'alternatives_internal'),
    attr(x,'alternatives')
  ) %>% as.data.frame(stringsAsFactors = FALSE)
}


vec_proxy_equal.cat_categorical<-function(x,...){
  active<-get_active_values(x)
  if(has_multiple_response(x)){stop('can only check equality on \'categorical\' vectors if exactly one level is selected for each item')}
  unlist(active)

}



