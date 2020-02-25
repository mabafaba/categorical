

vec_proxy_compare.cat_ordinal<-function(x,...){
  (alternate(x,'ranks',T) %>%
     unlist %>% as.numeric)
  }


