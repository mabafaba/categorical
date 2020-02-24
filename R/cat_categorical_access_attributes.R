
alternatives<-function(x, internal = FALSE){
  if(!is_categorical(x)){stop("not a <categorical> vector")}
  if(!internal){
    return(attributes(x)$alternatives)
  }

  return(attributes(x)$alternatives_internal)
}


