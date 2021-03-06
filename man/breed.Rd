% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breed.R
\name{breed}
\alias{breed}
\title{support function: breed()}
\usage{
breed(chromoSet, cross_cutNum = 1, mutation_prob = 0.01)
}
\arguments{
\item{chromoSet}{dataframe; both column corresponding to chromosomes; Each row of this data frame shows both parents}

\item{cross_cutNum}{integer; How many cuts do you want for the crossover? Defaults to 1}

\item{mutation_prob}{numeric; What is the probability of mutation? Defaults to 0.01}
}
\description{
This function put the crossover and mutation parts together. Aslo aims to make sure the alignment of the data type in each part applies the crossover() and mutation() functions to all chromosomes.
}
\examples{
breed()
}
\keyword{breed}
