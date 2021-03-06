% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_formula.r
\name{parse_power}
\alias{parse_power}
\title{Parse power specifications}
\usage{
parse_power(sim_args, samp_size)
}
\arguments{
\item{sim_args}{A named list with special model formula syntax. See details and examples
for more information. The named list may contain the following:
\itemize{
  \item fixed: This is the fixed portion of the model (i.e. covariates)
  \item random: This is the random portion of the model (i.e. random effects)
  \item error: This is the error (i.e. residual term).
}}

\item{samp_size}{The sample size pulled from the simulation arguments or the 
power model results when vary_arguments is used.}
}
\description{
Parse power specifications
}
