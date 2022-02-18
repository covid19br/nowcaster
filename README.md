
# nowcaster <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster.png' align="right" width="140" /></a> <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster_rev.png' align="right" width="140" /></a>

<p align="justify">`nowcaster` is a R package for “nowcasting” epidemiological time-series.
Every single system of notification has a intrinsic delay, `nowcaster`
can estimate how many counts of any epidemiological data of interest
(*i.e.*, daily cases and deaths counts) by fitting a binomial negative
model to the time steps of delay between onset date of the event,
(*i.e.*, date of first symptoms for cases or date of occurrence of
death) and the date of report (*i.e.*, date of notification of the case
or death)</>

`nowcaster` is based on the
[`R-INLA`](https://becarioprecario.bitbucket.io/inla-gitbook/index.html)
and
[`INLA`](https://inla.r-inla-download.org/r-inla.org/doc/inla-manual/inla-manual.pdf)
packages for “**I**ntegrated **N**ested **L**aplace **A**pproximation”
algorithm to Bayesian inference. `INLA` is an alternative to others
methods for Bayesian inference like **MCMC**, An introduction to `INLA`
can be found
[here](https://becarioprecario.bitbucket.io/inla-gitbook/index.html).

`nowcaster` is build for epidemiological emergency use, it was
constructed for the Brazilian Severe Acute Respiratory Illness (SARI)
surveillance database (SIVEP-Gripe).

## Installing

remotes::install_github(“covid19br/nowcaster”)
