# Build the Portuguese version of the pkgdown site into docs/pt.
#
# Run from the package root, after the English site has been built into docs/:
#   Rscript pkgdown/build-pt.R
#
# The Portuguese site is built from a temporary copy of the package in which
# the home page (index.md), the vignettes and _pkgdown.yml are replaced by the
# Portuguese versions kept in pkgdown/pt/. The function reference and the
# changelog are the English ones, with the site interface in Portuguese.

pkg <- normalizePath(".")
pt_src <- file.path(pkg, "pkgdown", "pt")
stopifnot(file.exists(file.path(pkg, "DESCRIPTION")), dir.exists(pt_src))

tmp <- file.path(tempdir(), "nowcaster-pt")
unlink(tmp, recursive = TRUE)
dir.create(tmp)

# Copy the package, leaving out the built site and version control
top <- list.files(pkg, all.files = TRUE, no.. = TRUE)
top <- setdiff(top, c("docs", ".git", ".Rproj.user"))
file.copy(file.path(pkg, top), tmp, recursive = TRUE)

# Swap in the Portuguese content
file.copy(file.path(pt_src, "_pkgdown.yml"), file.path(tmp, "_pkgdown.yml"),
          overwrite = TRUE)
file.copy(file.path(pt_src, "index.md"), file.path(tmp, "index.md"),
          overwrite = TRUE)
unlink(file.path(tmp, "vignettes"), recursive = TRUE)
file.copy(file.path(pt_src, "vignettes"), tmp, recursive = TRUE)

dest <- file.path(pkg, "docs", "pt")
unlink(dest, recursive = TRUE)

pkgdown::build_site(
  tmp,
  override = list(destination = dest),
  preview = FALSE,
  install = FALSE,
  new_process = FALSE
)
