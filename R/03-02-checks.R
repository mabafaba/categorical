


check_levels_match_public_alternatives_length<-function(x){
  length(levels(x)) == nrow(attributes(x)$alternatives)
}
