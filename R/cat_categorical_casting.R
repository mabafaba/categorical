
vec_cast.cat_categorical <- function(x, to, ...) UseMethod("vec_cast.cat_categorical")
vec_cast.cat_categorical.default <- function(x, to, ...) categorical(x, to)

vec_cast.cat_categorical.cat_categorical <- function(x, to, ...) categorical(x)
vec_cast.cat_categorical.integer <- function(x, to, ...) categorical(x)
vec_cast.cat_categorical.character <- function(x, to, ...) cagtegorical(x)

(categorical(c(1,2,3)))
browseVignettes('categorical')
