#' When the change happens implicitly (e.g in c()) we call it coercion
#' ec_ptype2(x, y) defines possible set of coercions.
#' It returns a prototype if x and y can be safely coerced to the same prototype;
#' otherwise it returns an error.
#' The set of automatic coercions is usually quite small
#' because too many tend to make code harder to reason about
#' and silently propagate mistakes.

#' each combo of our class and other classes that can be coerced toghether has
#' a function that returns just a prototype of the resulting class

#' start with the boilerplate for vec_ptype2()
#' this is just so later we can write vec_ptype2.cat_select_multiple.OTHERCLASS
#' to define how these two classes should be coerced together
vec_ptype2.cat_select_multiple <- function(x, y, ...) UseMethod("vec_ptype2.cat_select_multiple", y)
vec_ptype2.cat_select_multiple.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}


vec_cast.cat_select_multiple <- function(x, to, ...) UseMethod("vec_cast.cat_select_multiple")
vec_cast.cat_select_multiple.default <- function(x, to, ...) vec_default_cast(x, to)


vec_ptype2.cat_select_multiple.cat_select_multiple <- function(x, y, ...) {

  choices<-unique(c(levels(e1),levels(e2)))
  labels<-c(attributes(e1)$labels,attributes(e2)$labels)
  labels<-labels[!duplicated(names(labels))]
  values<-c(unclass(x),unclass(y))
  select_multiple(values,choices = choices,labels = labels)

}

vec_ptype2.cat_select_multiple.character <- function(x, y, ...) new_select_multiple()
vec_ptype2.cat_select_multiple.double <- function(x, y, ...) new_select_multiple()
vec_ptype2.cat_select_multiple.integer <- function(x, y, ...) new_select_multiple()
vec_ptype2.cat_select_multiple.list <- function(x, y, ...) new_select_multiple()
vec_ptype2.cat_select_multiple.cat_select_one <- function(x, y, ...) new_select_multiple()

vec_ptype2(select_multiple(), select_multiple())
vec_ptype_show(select_multiple(),integer(),double())



vec_ptype2.cat_select_multiple(x)
