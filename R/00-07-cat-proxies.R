
# vctrs proxies -----------------------------------------------------------
# define how the vector is converted before being used in many base functions such as logical operators `==`, `<` etc.

vec_proxy_equal.cat_categorical<-function(x,...){

  active<-get_active_values(x)
  if(all(purrr::map_dbl(active, length)==1)){
    return(unlist(active))
  }
  return(active)

}


