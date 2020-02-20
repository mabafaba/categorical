

vec_proxy_equal.cat_categorical<-function(x,...){
  # if(has_multiple_response(x)){stop('can only check equality on \'categorical\' vectors if exactly one level is selected for each item')}


  active<-get_active_values(x)
  if(all(purrr::map_dbl(active, length)==1)){
    return(unlist(active))
  }
  return(active)

}


