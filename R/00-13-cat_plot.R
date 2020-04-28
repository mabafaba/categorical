
#' plotting categorical vectors
#' @param x categorical vector
#' @export
#'
plot.cat_categorical<-function(x){

    if (requireNamespace("UpSetR", quietly = TRUE)) {
      # convert to matrix, convert that to a numeric data frame, then plot with upsetR
      UpSetR::upset(
        as.data.frame(
          apply(
            as.matrix(x),2,as.numeric)))
      } else {
      lgl<-as.matrix(x)
      barplot(colSums(lgl))
    }
  }
