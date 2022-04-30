
# nowcaster <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster.png' align="right" width="140" /></a> <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster_rev.png' align="right" width="140" /></a>

`nowcaster` is a R package for “nowcasting” epidemiological time-series.
Every single system of notification has an intrinsic delay, `nowcaster`
can estimate how many counts of any epidemiological data of interest
(*i.e.*, daily cases and deaths counts) by fitting a negative binomial
model to the time steps of delay between onset date of the event,
(*i.e.*, date of first symptoms for cases or date of occurrence of
death) and the date of report (*i.e.*, date of notification of the case
or death).

`nowcaster` is based on the
[`R-INLA`](https://becarioprecario.bitbucket.io/inla-gitbook/index.html)
and
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

To install `nowcaster` package simply run the code below in R:

``` r
devtools::install_github("https://github.com/covid19br/nowcaster")
```

After installing you can load the by typical library:

``` r
library(nowcaster)
```

## First example on LazyData

When the package is loaded it disponibilize a LazyData file, `sariBH`,
it is a annonymized records of Severe Acute Respiratory Illness notified
in the city of Belo Horizonte, since March 2020 to April 2022. To load
it basically write:

``` r
data<-sragBH
```

And we take a look on the data:

``` r
head(data)
```

    ##   DT_SIN_PRI  DT_DIGITA CLASSI_FIN EVOLUCAO CO_MUN_RES Idade fx_etaria
    ## 1 2020-02-11 2020-03-05          4        1     310620    59   50 - 59
    ## 2 2020-01-21 2020-02-06          4        1     310620    79   70 - 79
    ## 3 2020-03-30 2020-04-17          4        1     310620    72   70 - 79
    ## 4 2020-03-26 2020-04-02          4        1     310620    82      80 +
    ## 5 2020-03-20 2020-04-13          4        1     310620    50   50 - 59
    ## 6 2020-04-07 2020-04-22          5        1     310620    74   70 - 79

It is a data.frame with 7 variables and 65,404 observations. We will
make use of only the first two columns, “DT_SIN_PRI” (date of onset
symptoms) and “DT_DIGITA” (recording date) as well the column “Idade”
(age in years) to make age structured nowcasting.

## Non-structured data

Now we call the nowcasting function, it has by default the
parametrization to take the data and estimate with a non-structured data
form. The estimate fits a negative binomial distribution,
*N**e**g**B**i**n**o**m*(*λ*<sub>*t*, *d*</sub>,*ϕ*), to the cases count
at time *t* with delay *d*, *ϕ* is the dispersion parameter. The rate
*λ*<sub>*t*, *d*</sub> is then parameterized in a log-linear format by a
constant term added by structured delay random effects and structured
time random effects. Hence, the model isgiven by the following:
*Y*<sub>*t*, *d*</sub> ∼ *N**e**g**B**i**n**o**m*(*λ*<sub>*t*, *d*</sub>,*ϕ*),  *t* = 1, 2, …, *T*,  *d* = 1, 2, …, *D*,
log (*λ*<sub>*t*, *d*</sub>) = *α* + *β*<sub>*t*</sub> + *γ*<sub>*d*</sub>.
where the intercept *α* follows is Gaussian distribution with a very
large variance, *β*<sub>*t*</sub> is follows a second order random walk
with precision *τ*<sub>*β*</sub>, *γ*<sub>*d*</sub> a first-order random
walk with precision *τ*<sub>*γ*</sub>. The model is then completed by
INLA default prior distributions for *ϕ*, *τ*<sub>*β*</sub>, and
*τ*<sub>*γ*</sub>. See nbinom, rw1 and rw2 INLA help pages.

The call of the function is straightforward, it simply needs a dataset
as input, here the `LazyData` loaded in the namespace of the package.

``` r
nowcasting_bh_no_age <- nowcasting_inla(dataset = sragBH)
head(nowcasting_bh_no_age$total)
```

    ## # A tibble: 6 × 7
    ##    Time dt_event   Median    LI    LS   LIb   LSb
    ##   <int> <date>      <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    17 2021-12-13    625   621  633    623   627
    ## 2    18 2021-12-20    695   687  707    691   698
    ## 3    19 2021-12-27    812   801  829    808   817
    ## 4    20 2022-01-03    886   870  908    880   893
    ## 5    21 2022-01-10    817   800  843.   811   826
    ## 6    22 2022-01-17    631   609  660.   623   639

This calling will return only the nowcasting estimate and its Confidence
Interval (CI) for two different Credible interval, `LIb` and `LSb` are
the max and min CI, respectively, with credibility of 50% and `LI` and
`LS` are the max and min CI, respectively, with credibility of 95%.

`nowcasting_inla` has the option to return the curve out of the window
of action of the model, if the `data.by.week` is flagged as \``TRUE` it
returns on the third element of the output list the summarized data by
week.

``` r
nowcasting_bh_no_age <- nowcasting_inla(dataset = sragBH, data.by.week = T)
head(nowcasting_bh_no_age$dados)
```

    ## # A tibble: 6 × 3
    ##   DT_DIGITA  DT_SIN_PRI Delay
    ##   <date>     <date>     <dbl>
    ## 1 2021-01-04 2020-12-28     1
    ## 2 2021-01-11 2021-01-04     1
    ## 3 2021-01-11 2020-12-28     2
    ## 4 2021-01-11 2021-01-04     1
    ## 5 2021-01-11 2020-12-28     2
    ## 6 2021-01-18 2021-01-04     2

If this third element is groped by and summarized by the onset of
symptoms date, here `DT_SIN_PRI` it is the epidemiological curve. To
example it we plot the estimate and the epidemiological curve all
together.

``` r
library(ggplot2)

dados_by_week <- nowcasting_bh_no_age$dados %>% 
  filter(DT_SIN_PRI >= (Sys.Date()-270)) %>% 
  group_by(DT_SIN_PRI) %>% 
  summarise(n = n())


nowcasting_bh_no_age$total %>% 
  ggplot(aes(x = dt_event, y = Median, col = 'Nowcasting')) +
  geom_line(data = dados_by_week, aes(DT_SIN_PRI, y = n, col = 'Observed'))+
  geom_ribbon(aes(ymin = LI, ymax = LS, col = NA), alpha = 0.2, show.legend = F)+
  geom_line()+
  theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c('grey50', 'black'), name = '')+
  scale_x_date(date_breaks = '2 weeks', date_labels = '%V/%y', name = 'Date in Weeks')+
  labs(x = '', y = 'Nº Cases')
```

![](README_files/figure-gfm/no_age_plot-1.png)<!-- -->

## Structured data, Age

For the structured data the `nowcasting_inla()` fits again a Negative
binomial distribution to the cases count at time *t* with delay *d*.
Differently, from the non-structured case the model now gives random
effects to the delay distribution and and time distribution by each of
the age-class chosen by the user to break the data. The model has the
form now:

$$Y\_{t,d,a} \\sim  NegBinom(\\lambda\_{t,d,a}, \\phi), \\quad t=1,2,\\ldots,T, \\quad d=1,2,\\ldots,D, $a=1,2,\\ldots,A$$
$
log (*λ*<sub>*t*, *d*, *a*</sub>) = *α*<sub>*a*</sub> + *β*<sub>*t*, *a*</sub> + *γ*<sub>*d*, *a*</sub>.
where each age class, *a*, has an intercept *α*<sub>*a*</sub> following
a Gaussian distribution with a very large variance, the time-age random
effects, *β*<sub>*t*, *a*</sub>, follow a joint multivariate Gaussian
distribution with a separable variance components an independent
Gaussian term for the age classes with precision
*τ*<sub>*a**g**e*, *β*</sub> and a second order random walk term for the
time with precision *τ*<sub>*β*</sub>. Analogously, the delay-age random
effects, *γ*<sub>*d*, *a*</sub>, follow a joint multivariate Gaussian
distribution with a separable variance components an independent
Gaussian term for the age classes with precision
*τ*<sub>*a**g**e*, *γ*</sub> and a first order random walk term for the
time with precision *τ*<sub>*γ*</sub>. The model is then completed by
INLA default prior distributions for *ϕ*, *τ*<sub>*a**g**e*, *β*</sub>,
*τ*<sub>*a**g**e*, *γ*</sub>, *τ*<sub>*β*</sub> and *τ*<sub>*γ*</sub>.
See nbinom, iid, rw1 and rw2 INLA help pages.

This new model corrects the delay taking into account the effects of age
classes and the interactions of each age class between time and also
delay. Now the model needs a flag indicating which is the column on the
dataset which will be used to break the data into age classes and how
the age classes will be split. This is given by the parameters `age_col`
and `bins_age`. We pass three additional parameters, `data.by.week` to
return the epidemiological curve out of window of action of nowcasting
estimate and `return.age` to inform we desire a nowcasting result in two
ways, the total aggregation estimate and the age-stratified estimate.
The calling of the function has the following form:

``` r
nowcasting_bh_age <- nowcasting_inla(dataset = sragBH, 
                                   bins_age = "10 years",
                                   data.by.week = T, 
                                   age_col = Idade)
```

Each of the estimates returned by `nowcasting_inla` has the same form as
in the non-structured case. On the nowcasting estimates, it returns a
data.frame with the posterior edian and 50% and 95% credible intervals,
(LIb and LSb) and (LI and LS) respectively.

``` r
library(ggplot2)

dados_by_week <- nowcasting_bh_age$dados %>% 
  filter(DT_SIN_PRI >= (Sys.Date()-270)) %>% 
  group_by(DT_SIN_PRI) %>% 
  summarise(n = n())


nowcasting_bh_age$total %>% 
  ggplot(aes(x = dt_event, y = Median, col = 'Median'))+
  geom_line()+
  geom_line(data = dados_by_week, aes(DT_SIN_PRI, y = n))+
  geom_ribbon(aes(ymin = LI, ymax = LS, col = 'IC'), alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = c('grey90', 'black'), name = '')+
  scale_x_date(date_breaks = '2 weeks', date_labels = '%V/%y', name = 'Date in Weeks')+
  labs(x = '', y = 'Nº Cases')
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

## Comparing the estimates

We can compare the estimates by each of the strategies, we plot the two
estimates together:

``` r
nowcasting_bh_no_age$total$type <- "No Age structured"
nowcasting_bh_age$total$type <- "Age structured"


nowcasting_bh_total <- nowcasting_bh_age$total %>% 
  full_join(nowcasting_bh_no_age$total)

nowcasting_bh_total %>% 
  ggplot(aes(x = dt_event, y = Median, col = type))+
  geom_line(show.legend = F)+
  geom_ribbon(aes(ymin = LI, ymax = LS, fill = NULL), alpha = 0,
              show.legend = T)+
  # geom_line(data = dados_by_week, aes(DT_SIN_PRI, y = n))+
  theme_bw()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = c('grey60', 'grey90'), name = '')+
  scale_fill_manual(values = c('grey60', 'grey90'), name = '')+
  scale_x_date(date_breaks = '2 weeks', date_labels = '%V/%y', name = 'Date in Weeks')+
  labs(x = '', y = 'Nº Cases')
```

![](README_files/figure-gfm/compare_plot-1.png)<!-- -->

The estimates gives different CIs, this is due to a better fitting when
considering random effects by age class for the delays at time, this has
to do with the different capability to respond on different ages. This
is an empirical finding of this models.

``` r
sessionInfo()
```

    ## R version 4.1.3 (2022-03-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Manjaro Linux
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/libblas.so.3.10.0
    ## LAPACK: /usr/lib/liblapack.so.3.10.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] INLA_22.04.06   sp_1.4-6        foreach_1.5.2   Matrix_1.4-0   
    ##  [5] lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0   dplyr_1.0.9    
    ##  [9] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.6   
    ## [13] ggplot2_3.3.5   tidyverse_1.3.1 nowcaster_0.1.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] fs_1.5.2            usethis_2.1.5       devtools_2.4.3     
    ##  [4] httr_1.4.2          rprojroot_2.0.2     numDeriv_2016.8-1.1
    ##  [7] tools_4.1.3         backports_1.4.1     utf8_1.2.2         
    ## [10] R6_2.5.1            sn_2.0.1            DBI_1.1.2          
    ## [13] colorspace_2.0-3    withr_2.5.0         mnormt_2.0.2       
    ## [16] tidyselect_1.1.2    prettyunits_1.1.1   processx_3.5.2     
    ## [19] curl_4.3.2          compiler_4.1.3      cli_3.3.0          
    ## [22] rvest_1.0.2         xml2_1.3.3          desc_1.4.0         
    ## [25] labeling_0.4.2      scales_1.1.1        callr_3.7.0        
    ## [28] digest_0.6.29       rmarkdown_2.11      pkgconfig_2.0.3    
    ## [31] htmltools_0.5.2     sessioninfo_1.2.2   highr_0.9          
    ## [34] dbplyr_2.1.1        fastmap_1.1.0       rlang_1.0.2        
    ## [37] readxl_1.3.1        rstudioapi_0.13     farver_2.1.0       
    ## [40] generics_0.1.2      jsonlite_1.7.3      magrittr_2.0.3     
    ## [43] Rcpp_1.0.8          munsell_0.5.0       fansi_1.0.3        
    ## [46] lifecycle_1.0.1     stringi_1.7.6       yaml_2.3.4         
    ## [49] brio_1.1.3          pkgbuild_1.3.1      grid_4.1.3         
    ## [52] crayon_1.5.1        lattice_0.20-45     haven_2.4.3        
    ## [55] splines_4.1.3       hms_1.1.1           tmvnsim_1.0-2      
    ## [58] knitr_1.37          ps_1.6.0            pillar_1.7.0       
    ## [61] stats4_4.1.3        codetools_0.2-18    pkgload_1.2.4      
    ## [64] reprex_2.0.1        glue_1.6.2          evaluate_0.15      
    ## [67] remotes_2.4.2       modelr_0.1.8        vctrs_0.4.1        
    ## [70] tzdb_0.2.0          MatrixModels_0.5-0  testthat_3.1.2     
    ## [73] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
    ## [76] cachem_1.0.6        xfun_0.29           broom_0.7.12       
    ## [79] iterators_1.0.14    memoise_2.0.1       ellipsis_0.3.2
