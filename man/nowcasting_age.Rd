% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nowcasting_age.R
\name{nowcasting_age}
\alias{nowcasting_age}
\title{nowcasting_age}
\usage{
nowcasting_age(
  dataset,
  zero_inflated = FALSE,
  timeREmodel = "rw2",
  INLAoutput = F,
  INLAoutputOnly = F,
  WAIC = F,
  DIC = F
)
}
\arguments{
\item{dataset}{data pre formatted in to age classes and delays by week for each cases, delay triangle format}

\item{zero_inflated}{Experimental! It deals with zero-inflated data by using a zeroinflatednbinomial2 model.}

\item{timeREmodel}{Latent model for time random effects. The default is a second-order random walk model.}

\item{INLAoutput}{return the INLA output. Default is FALSE.}

\item{INLAoutputOnly}{return the only the INLA output. Default is FALSE.}

\item{WAIC}{return the WAIC. Default is FALSE.}

\item{DIC}{return the DIC. Default is FALSE.}
}
\value{
Trajectories from the inner 'INLA' model
}
\description{
Run INLA model on structured data by age-class
data has to be in the format of delay-triangle
}
