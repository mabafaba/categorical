context("test-trailanderror")

library(categorical)
library(dplyr)

test<-c('A B', 'A B C')
x<-select_multiple(test)
undebug(print.data.frame)

format.cat_select_multiple

data.frame(a=x)

