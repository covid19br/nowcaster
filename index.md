index
================

[![](https://img.shields.io/badge/devel%20version-0.2.1-blue.svg)](https://github.com/nowcaster)
<!-- [![](https://img.shields.io/badge/download-NA/total-blue.svg)](https://bioconductor.org/packages/stats/bioc/nowcaster)  -->
<!-- [![R build status](https://github.com/covid19br/nowcaster/workflows/R-CMD-check/badge.svg)](https://github.com/covid19br/nowcaster/actions) -->

`nowcaster` is a R package for “nowcasting” epidemiological time-series.
Every single system of notification has an intrinsic delay, `nowcaster`
can estimate how many counts of any epidemiological data of interest
(*i.e.*, daily cases and deaths counts) by fitting a negative binomial
model to the time steps of delay between onset date of the event,
(*i.e.*, date of first symptoms for cases or date of occurrence of
death) and the date of report (*i.e.*, date of notification of the case
or death).

`nowcaster` is based on the
[`R-INLA`](https://www.r-inla.org/download-install) and
[`INLA`](https://inla.r-inla-download.org/r-inla.org/doc/inla-manual/inla-manual.pdf)
packages for “**I**ntegrated **N**ested **L**aplace **A**pproximation”
algorithm to Bayesian inference. `INLA` is a fast alternative to others
methods for Bayesian inference like **MCMC**. An introduction to `INLA`
can be found
[here](https://becarioprecario.bitbucket.io/inla-gitbook/index.html).

`nowcaster` is build for epidemiological emergency use, it was
constructed for the Brazilian Severe Acute Respiratory Illness (SARI)
surveillance database (SIVEP-Gripe).

## Installing

Before installing the package certify you have an active installation of
`INLA`, to do so you can run the following code:

If you want more detail on other possible installations of `INLA`,
please refer to the official
[page](https://www.r-inla.org/download-install) of the package.

After have a proper `INLA` installation to install `nowcaster` package
simply run the code below in R:
