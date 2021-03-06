% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-00-cat.R
\name{mutate_categorical}
\alias{mutate_categorical}
\title{Mutate categorical type variables in a data frame}
\usage{
mutate_categorical(.data, ...)
}
\arguments{
\item{.data}{a data.frame or tibble}

\item{...}{arguments passed to dplyr::mutate}
}
\value{
see ?dplyr::mutate
}
\description{
Mutate categorical type variables in a data frame
}
\details{
operates rowwise (see ?dplyr::rowwise) on a categorical column. Each row's value is a vector with the selected responses.
}
