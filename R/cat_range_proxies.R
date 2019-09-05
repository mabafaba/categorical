

vec_proxy_compare.cat_range<-function(x,...){
  (alternate(x,'ranks',T) %>%
     unlist %>% as.numeric)
}


