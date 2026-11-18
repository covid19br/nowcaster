# SARI data from Belo Horizonte

An anonymized dataset containing cases of severe acute respiratory
illness (SARI) from a Brazilian municipality, Belo Horizonte, with
symptoms onset varying from 2019-12-29 to 2022-03-27.

## Usage

``` r
sragBH
```

## Format

A data frame with 65404 rows and 7 variables:

- DT_SIN_PRI:

  Date of onset symptoms.

- DT_DIGITA:

  Date of recording.

- CLASSI_FIN:

  Final classification of the case. (Not used for nowcasting)

- EVOLUCAO:

  Case evolution. (Not used for nowcasting)

- CO_MUN_RES:

  IBGE municipality code. Belo Horizonte is 310620.

- Idade:

  Age in years.

- fx_etaria:

  Age brackets.

## Source

<https://opendatasus.saude.gov.br/>
