is_same<-function(x,y){
  # for NA comparison: they can't be the same if exactly one of them is NA (not neither, not both):
  sameness<-rep(NA,length(x))
  equals <- x==y
  both_na <- is.na(x) & is.na(y)
  only_x_na <- is.na(x) & !is.na(y)
  only_y_na <- is.na(y) & !is.na(x)


  sameness[only_x_na]<-FALSE
  sameness[only_y_na]<-FALSE
  sameness[both_na]<-TRUE
  sameness[which(equals)]<-TRUE
  sameness[which(!equals)]<-FALSE

  sameness
}



