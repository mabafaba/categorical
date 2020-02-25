
plot.cat_categorical<-function(x){

  lgl<-as.matrix(x)
  par(mfrow=c(2,2))
  barplot(colSums(lgl))

}
