#' Deprecated functions in nowcaster
#'
#' @description
#' These functions were renamed so that functions used by only one of the
#' workhorse functions end in `_inla` or `_mgcv`. They still work but give a
#' deprecation warning and will be removed in a future release.
#'
#' * `nowcasting_no_age()` is now [nowcasting_no_age_inla()].
#' * `nowcasting_age()` is now [nowcasting_age_inla()].
#'
#' @param ... Arguments passed to the new function.
#'
#' @return The output of the new function.
#' @name nowcaster-deprecated
#' @keywords internal
NULL

#' @rdname nowcaster-deprecated
#' @export
nowcasting_no_age <- function(...) {
  .Deprecated("nowcasting_no_age_inla", package = "nowcaster")
  nowcasting_no_age_inla(...)
}

#' @rdname nowcaster-deprecated
#' @export
nowcasting_age <- function(...) {
  .Deprecated("nowcasting_age_inla", package = "nowcaster")
  nowcasting_age_inla(...)
}
