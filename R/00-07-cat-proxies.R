
# vctrs proxies -----------------------------------------------------------
# define how the vector is converted before being used in many base functions such as logical operators `==`, `<` etc.

vec_proxy_equal.cat_categorical<-function(x,...){

  level<-get_level_values(x)
  if(all(purrr::map_dbl(level, length)==1)){
    return(unlist(level))
  }
  return(level)

}


