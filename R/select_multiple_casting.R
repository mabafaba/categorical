#' vec_cast(x, to) defines the possible sets of casts.
#' It returns x translated to have prototype to, or throws an error if the conversion isn’t possible.
#' The set of possible casts is a superset of possible coercions
#' because they’re requested explicitly.

#' casting rules:
#'
#' generally keep all all original levels and unique values as levels; keep first label for each unique level (warning if conflicting?????)
#' select_multiple from double >>> error
#' select_multiple from integer >>> error
#' select_multiple from date >>> error
#' select_multiple from factor >>> select_multiple;
#' select_multiple from select_one >>> select_multiple
#' select_multiple from character >>> select_multiple (characters as select one choices)
#' select_one >>> select_multiple
#'
#'
#' select_multiple to list >>> simple list; levels & labels discarded
#'
#' select_multiple to atomic.. some fundamental decisions to be made..
#' option 1: keep as a list and cast each item using base R rules
#'     + preserves size
#'     + preserves info which response came from which record
#'     + if select_multiple allows different base types (?????),
#'       then this would be the expected behaviour in lots of cases;
#'      the goal anyway is to make select_multiples behave like regular vectors as much as possible.
#'      (maybe this line of thoughts warrants something like as_select_multiple_character().. ? --> fundamental decision whether or not to allow different base types?????)
#'     - _does not return an object of the requested class_ !
#'
#' option 2: 'unlist' into single vector with all elements; then cast with base R rules.
#'     + creates a vector of the requested class
#'     - does not preserve length
#'     - does not preserve which response came from which record
#'
#' option 3: require additional parameter which of the above options should be used?
#'     + makes this someone elses problem
#'     - makes this someone elses problem
#'
#' option 4: throw an error
#'     + strict & somewhat makes sense;
#'     - people will _have_ to be able to convert back to basic types.
#'     - it's a massive abstraction leak; without this option the only way out is to understand how select_multiples are built internally and some basic knowledge of apply/purrr to handle
#'
#' option 5: allow only as.character: concatenate everything into characters and concatenate
#'     - how to create/provide a reliable separator?
#'     - if separator is used in choices, problem
#'     - the main way forward from there is probably a strsplit, so as.character is just an less safe detour of what as.list would do
#'
#' further thoughts:
#'     - select_multiples that only have one level or where at most one level is selected in each record should pose no problem.
#'     - maybe one should have to chose one of the choices with it
#'         - as.logical(x, 'option1') becomes c(TRUE, FALSE, TRUE))
#'         - as.character(x, 'option1') becomes c("option1", "" , "option1"))
#'         - as.factor(x, 'option1') becomes factor("option1", NA, "option1"))
#'         - etc.
#'    - maybe direct coersion just shouldn't be a thing you would usually do with a select_multiple vector, and instead you would always have to take a detour:
#'         - convert to logical matrix and take it from there
#'         - collapse into single selection first and take it from there (something like forcats::fct_collapse() but considering combinations)




#' unlike the coersions, the casting functions return _the casted object_ and not just the
vec_cast.cat_select_multiple <- function(x, to, ...) UseMethod("vec_cast.cat_select_multiple")
vec_cast.cat_select_multiple.default <- function(x, to, ...) vec_default_cast(x, to)

vec_cast.cat_select_multiple.cat_select_multiple <- function(x, to, ...) x
vec_cast.cat_select_multiple.integer <- function(x, to, ...) select_multiple(x)
vec_cast.cat_select_multiple.character <- function(x, to, ...) select_multiple(x)
# vec_cast.double.cat_select_multiple <- function(x, to, ...) vec_data(x) ?
