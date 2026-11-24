# Deprecated functions in nowcaster

These functions were renamed so that functions used by only one of the
workhorse functions end in `_inla` or `_mgcv`. They still work but give
a deprecation warning and will be removed in a future release.

- `nowcasting_no_age()` is now
  [`nowcasting_no_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_no_age_inla.md).

- `nowcasting_age()` is now
  [`nowcasting_age_inla()`](https://covid19br.github.io/nowcaster/pt/reference/nowcasting_age_inla.md).

## Uso

``` r
nowcasting_no_age(...)

nowcasting_age(...)
```

## Argumentos

- ...:

  Arguments passed to the new function.

## Valor

The output of the new function.
