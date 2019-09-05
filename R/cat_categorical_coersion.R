vec_ptype2.cat_categorical <- function(x, y, ...) UseMethod("vec_ptype2.cat_categorical", y)
vec_ptype2.cat_categorical.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

vec_ptype2.cat_categorical.cat_categorical <- function(x, y, ...) categorical()



