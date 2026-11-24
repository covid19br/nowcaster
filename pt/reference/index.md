# Índice de pacotes

## Nowcasting

Funções principais. As duas ajustam um modelo binomial negativo ao
triângulo de atraso;
[`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md)
usa o ‘INLA’ e
[`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_mgcv.md)
usa o ‘mgcv’. A documentação das funções está em inglês.

- [`nowcasting_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_inla.md)
  : Nowcasting with a Bayesian negative binomial model fitted by 'INLA'
- [`nowcasting_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_mgcv.md)
  : Nowcasting with a negative binomial GAM fitted by 'mgcv'

## Ajuste dos modelos

Funções de nível mais baixo, chamadas pelas funções principais.

- [`nowcasting_no_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_no_age_inla.md)
  : Fit the non-structured nowcasting model with 'INLA'
- [`nowcasting_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_age_inla.md)
  : Fit the structured (age) nowcasting model with 'INLA'
- [`nowcasting_no_age_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_no_age_mgcv.md)
  : Fit the non-structured nowcasting GAM
- [`nowcasting_age_mgcv()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_age_mgcv.md)
  : Fit the structured (age) nowcasting GAM

## Pós-processamento

- [`nowcasting.summary()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting.summary.md)
  : Summarise nowcasting trajectories
- [`slope.estimate.quant()`](https://covid19br.github.io/nowcaster/pt/reference/slope.estimate.quant.md)
  : Estimate the slope of the nowcasted curve

## Dados

- [`sragBH`](https://covid19br.github.io/nowcaster/pt/reference/sragBH.md)
  : SARI data from Belo Horizonte
