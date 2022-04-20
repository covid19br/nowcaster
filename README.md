
# nowcaster <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster.png' align="right" width="140" /></a> <a href='https://github.com/covid19br/nowcaster'><img src='man/figures/nowcaster_rev.png' align="right" width="140" /></a>

`nowcaster` is a R package for “nowcasting” epidemiological time-series.
Every single system of notification has a intrinsic delay, `nowcaster`
can estimate how many counts of any epidemiological data of interest
(*i.e.*, daily cases and deaths counts) by fitting a binomial negative
model to the time steps of delay between onset date of the event,
(*i.e.*, date of first symptoms for cases or date of occurrence of
death) and the date of report (*i.e.*, date of notification of the case
or death)

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

To install `nowcaster` package simply run the code below in R:

``` r
devtools::install_github("https://github.com/covid19br/nowcaster")
```

    ## Skipping install of 'nowcaster' from a github remote, the SHA1 (06224452) has not changed since last install.
    ##   Use `force = TRUE` to force installation

After installing you can load the by typical library:

``` r
library(nowcaster)
```

## First example on LazyData

When the package is loaded it disponibilize a LazyData file, `sariBH`,
it is a annonymized records of Severe Acute Respiratory Illness notified
in the city of Belo Horizonte, since March 2020. To load it basically
write:

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
make use of only the first two columns, “DT\_SIN\_PRI” and “DT\_DIGITA”
as well the column “Idade” to make age structured nowcasting.

## Non-structured data

Now we call the nowcasting function, it has by default the
parametrization to take the data and estimate with a non-structured data
form. The estimate fits a
![NegBinom(\\lambda\_{\\tau}, t)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;NegBinom%28%5Clambda_%7B%5Ctau%7D%2C%20t%29 "NegBinom(\lambda_{\tau}, t)")
distribution to the cases count at time
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
with delay
![\\lambda\_\\tau](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_%5Ctau "\lambda_\tau").
The distribution is parametrize through assuming a random effect to the
delay
![\\lambda\_\\tau](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_%5Ctau "\lambda_\tau")
distribution given by a
![rw1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;rw1 "rw1")
and a random effect to the time
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
given by a
![rw2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;rw2 "rw2").
So, the model is:

![Y\_t \\sim  1 + rw2(t) + rw1(delay)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_t%20%5Csim%20%201%20%2B%20rw2%28t%29%20%2B%20rw1%28delay%29 "Y_t \sim  1 + rw2(t) + rw1(delay)")

The call of the function is straightforward, it simply needs a dataset
as input, here the `LazyData` loaded in the namespace of the package.

``` r
nowcasting_bh_no_age<-nowcasting_inla(dataset = data)
```

    ## Carregando pacotes exigidos: tidyverse

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Warning in nowcasting_inla(dataset = data): Using 'SI-PNI' age bins!

    ## Warning in nowcasting_inla(dataset = data): Using default to trim dates,
    ## trim.data = 0

    ## Warning in nowcasting_inla(dataset = data): Using default to maximum delay, Dmax
    ## = 15

    ## Warning in nowcasting_inla(dataset = data): Using default to window of action,
    ## wdw = 30

    ## Warning in nowcasting_inla(dataset = data): Using default to returning option
    ## for the data, data.by.week = FALSE

    ## Warning in nowcasting_inla(dataset = data): Using default to returning estimate
    ## by age, return.age = TRUE

    ## Warning in nowcasting_inla(dataset = data): Age_col missing, nowcasting with
    ## unstructured data

    ## Carregando pacotes exigidos: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Joining, by = "DT_SIN_PRI"
    ## Joining, by = c("DT_SIN_PRI", "delay", "Time")
    ## Carregando pacotes exigidos: INLA
    ## Carregando pacotes exigidos: Matrix
    ## Attaching package: 'Matrix'
    ## The following objects are masked from 'package:tidyr':
    ## 
    ## expand, pack, unpack
    ## Carregando pacotes exigidos: foreach
    ## Attaching package: 'foreach'
    ## The following objects are masked from 'package:purrr':
    ## 
    ## accumulate, when
    ## Carregando pacotes exigidos: parallel
    ## Carregando pacotes exigidos: sp
    ## This is INLA_21.11.22 built 2021-11-21 16:10:15 UTC. - See
    ## www.r-inla.org/contact-us for how to get help. - Save 80Mb of storage running
    ## 'inla.prune()'

``` r
head(nowcasting_bh_no_age$total)
```

    ## # A tibble: 6 x 7
    ##    Time dt_event   Median    LI    LS   LIb   LSb
    ##   <int> <date>      <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    17 2021-12-13    625  621   632    623   627
    ## 2    18 2021-12-20    695  687   707    692   698
    ## 3    19 2021-12-27    812  800   831    808   817
    ## 4    20 2022-01-03    886  870   910.   880   893
    ## 5    21 2022-01-10    818  800.  844    810   826
    ## 6    22 2022-01-17    631  611.  658.   623   640

This calling will return only the nowcasting estimate and its Confidence
Interval (CI) for two different Credible interval, `LIb` and `LSb` are
the max and min CI, respectively, with credibility of 50% and `LI` and
`LS` are the max and min CI, respectively, with credibility of 95%.

`nowcasting_inla` has the option to return the curve out of the window
of action of the model, if the `data.by.week` is flagged as \``TRUE` it
returns on the third element of the output list the summarized data by
week.

``` r
nowcasting_bh_no_age <- nowcasting_inla(dataset = data, data.by.week = T)
```

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Using 'SI-PNI' age
    ## bins!

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Using default to
    ## trim dates, trim.data = 0

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Using default to
    ## maximum delay, Dmax = 15

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Using default to
    ## window of action, wdw = 30

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Using default to
    ## returning estimate by age, return.age = TRUE

    ## Warning in nowcasting_inla(dataset = data, data.by.week = T): Age_col missing,
    ## nowcasting with unstructured data

    ## Joining, by = "DT_SIN_PRI"
    ## Joining, by = c("DT_SIN_PRI", "delay", "Time")

``` r
head(nowcasting_bh_no_age$dados)
```

    ## # A tibble: 6 x 3
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

dados_by_week<-nowcasting_bh_no_age$dados %>% 
  filter(DT_SIN_PRI >= (Sys.Date()-270)) %>% 
  group_by(DT_SIN_PRI) %>% 
  summarise(n = n())


nowcasting_bh_no_age$total %>% 
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

![](README_files/figure-gfm/no_age_plot-1.png)<!-- -->

## Structured data, Age

For the structured data the `nowcasting_inla()` fits again a
![NegBinom](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;NegBinom "NegBinom")
distribution to the cases count at time
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
with delay
![\\tau](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctau "\tau").
Differently, from the non-structured case the model now gives random
effects to the delay distribution and and time distribution by each of
the age-class choosed by the user to break the data. The model has the
form now:

![Y\_{t, Age} \\sim  1 + Age + rw2(t \| Age) + rw1(delay\|Age)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_%7Bt%2C%20Age%7D%20%5Csim%20%201%20%2B%20Age%20%2B%20rw2%28t%20%7C%20Age%29%20%2B%20rw1%28delay%7CAge%29 "Y_{t, Age} \sim  1 + Age + rw2(t | Age) + rw1(delay|Age)")

This new model gives to each of the age classes which the data was
divide a model poundering the delay effects and time by each age class
at time
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t").
Now the model needs a flag indicating which is the column on the dataset
which will be used to break the data into age classes and which is the
choosed option into which the age classes will be, this is given by the
parameters `age_col` and `bins_age`. We pass three additional
parameters, `data.by.week` to return the epidemiological curve out of
window of action of nowcasting estimate and `return.age` to inform we
desire a nowcasting result in two ways, the total aggregation estimate
and the age-stratified estimate. The calling of the function has the
following form:

``` r
nowcasting_bh_age<-nowcasting_inla(dataset = data, 
                                   bins_age = "10 years",
                                   data.by.week = T, 
                                   age_col = Idade)
```

    ## Warning in nowcasting_inla(dataset = data, bins_age = "10 years", data.by.week =
    ## T, : Using default to trim dates, trim.data = 0

    ## Warning in nowcasting_inla(dataset = data, bins_age = "10 years", data.by.week =
    ## T, : Using default to maximum delay, Dmax = 15

    ## Warning in nowcasting_inla(dataset = data, bins_age = "10 years", data.by.week =
    ## T, : Using default to window of action, wdw = 30

    ## Warning in nowcasting_inla(dataset = data, bins_age = "10 years", data.by.week =
    ## T, : Using default to returning estimate by age, return.age = TRUE

    ## Warning in dados.w(dataset = dados, bins_age = bins_age, trim.data =
    ## trim.data, : Using default, trimming out 0 days of data

    ## Warning in dados.w(dataset = dados, bins_age = bins_age, trim.data =
    ## trim.data, : Bins age in 10 years: 0 10 20 30 40 50 60 70 80 90+

    ## Joining, by = "DT_SIN_PRI"
    ## Joining, by = c("DT_SIN_PRI", "delay", "fx_etaria", "Time")

Each of the estimates returned by `nowcasting_inla` has the same form as
in the non-structured case, on the nowcasting estimates it returns a
data.frame with the Median and its CI by credibility of 50% (LIb and
LSb) and 95% (LI and LS).

``` r
library(ggplot2)

dados_by_week<-nowcasting_bh_age$dados %>% 
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
nowcasting_bh_no_age$total$type<-"No Age structured"
nowcasting_bh_age$total$type<-"Age structured"


nowcasting_bh_total<-nowcasting_bh_age$total %>% 
  full_join(nowcasting_bh_no_age$total)
```

    ## Joining, by = c("Time", "dt_event", "Median", "LI", "LS", "LIb", "LSb", "type")

``` r
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

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19042)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252   
    ## [3] LC_MONETARY=Portuguese_Brazil.1252 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.1252    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] INLA_21.11.22        sp_1.4-6             foreach_1.5.2       
    ##  [4] Matrix_1.3-4         lubridate_1.8.0      forcats_0.5.1       
    ##  [7] stringr_1.4.0        dplyr_1.0.8          purrr_0.3.4         
    ## [10] readr_2.1.2          tidyr_1.2.0          tibble_3.1.6        
    ## [13] ggplot2_3.3.5        tidyverse_1.3.1      nowcaster_0.0.0.9000
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] fs_1.5.2            usethis_2.1.5       devtools_2.4.3     
    ##  [4] httr_1.4.2          rprojroot_2.0.2     numDeriv_2016.8-1.1
    ##  [7] tools_4.1.2         backports_1.4.1     utf8_1.2.2         
    ## [10] R6_2.5.1            sn_2.0.1            DBI_1.1.2          
    ## [13] colorspace_2.0-2    withr_2.5.0         mnormt_2.0.2       
    ## [16] tidyselect_1.1.2    prettyunits_1.1.1   processx_3.5.2     
    ## [19] curl_4.3.2          compiler_4.1.2      cli_3.1.1          
    ## [22] rvest_1.0.2         xml2_1.3.3          desc_1.4.0         
    ## [25] labeling_0.4.2      scales_1.1.1        callr_3.7.0        
    ## [28] digest_0.6.29       rmarkdown_2.13      pkgconfig_2.0.3    
    ## [31] htmltools_0.5.2     sessioninfo_1.2.2   highr_0.9          
    ## [34] dbplyr_2.1.1        fastmap_1.1.0       rlang_1.0.1        
    ## [37] readxl_1.3.1        rstudioapi_0.13     farver_2.1.0       
    ## [40] generics_0.1.2      jsonlite_1.7.3      magrittr_2.0.2     
    ## [43] Rcpp_1.0.7          munsell_0.5.0       fansi_1.0.2        
    ## [46] lifecycle_1.0.1     stringi_1.7.6       yaml_2.2.2         
    ## [49] brio_1.1.3          pkgbuild_1.3.1      grid_4.1.2         
    ## [52] crayon_1.5.1        lattice_0.20-45     haven_2.4.3        
    ## [55] splines_4.1.2       hms_1.1.1           tmvnsim_1.0-2      
    ## [58] knitr_1.38          ps_1.6.0            pillar_1.7.0       
    ## [61] stats4_4.1.2        codetools_0.2-18    pkgload_1.2.4      
    ## [64] reprex_2.0.1        glue_1.6.1          evaluate_0.15      
    ## [67] remotes_2.4.2       modelr_0.1.8        vctrs_0.3.8        
    ## [70] tzdb_0.2.0          MatrixModels_0.5-0  testthat_3.1.3     
    ## [73] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
    ## [76] cachem_1.0.6        xfun_0.29           broom_0.7.12       
    ## [79] iterators_1.0.13    memoise_2.0.1       ellipsis_0.3.2
