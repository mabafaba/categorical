# join_active_alternative<-function(x,y){
#   alt_x<-get_active_alternative_name(x)
#   alt_y<-get_active_alternative_name(y)
#   if(length(alt_x)==0){
#     cat_to_use<-y
#   }else if(length(alt_y)==0){
#     cat_to_use<-x
#     }else{
#       cat_to_use<-x
#       }


#   active<-get_active_alternative_name(cat_to_use)
#   attributes(active)$is_internal<-get_active_alternative_is_internal(cat_to_use)
#   if(length(attributes(active)$is_internal)==0){
#     attributes(active)$is_internal<-FALSE
#   }
#   return(active)
# }



# levels and alternatives -------------------------------------------------

# get_active_alternative_name<-function(x){
#   alt<-attributes(x)[["active_alternative"]]
#   if(length(alt)==0){return(c())}
#   names(alt)<-ifelse(attributes(x)[["active_alternative_is_internal"]],"internal","public")
#   if(is.null(alt)){return(character())}
#   alt
# }

# get_active_alternative_is_internal<-function(x){
#   # no active alternative is always considered not internal:
#   if(length(get_active_alternative_name(x))==0){return(FALSE)}
#   return(attributes(x)$active_alternative_is_internal)
# }
