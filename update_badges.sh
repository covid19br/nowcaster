#!/bin/bash

# Script to update badges in README and index files
# This script regenerates the R markdown files to update badges with current package information

echo "Installing required R packages..."
R --vanilla -e "
if (!require('badger', quietly = TRUE)) install.packages('badger')
if (!require('rmarkdown', quietly = TRUE)) install.packages('rmarkdown')
"

echo "Generating badges to verify they work..."
R --vanilla -e "
library(badger)
cat('Current version badge:', badge_devel(color = 'blue'), '\n')
cat('License badge:', badge_license(url = 'https://github.com/covid19br/nowcaster/blob/main/LICENSE.md'), '\n')
cat('Lifecycle badge:', badge_lifecycle(stage = 'experimental'), '\n')
"

echo "Rendering R markdown files..."
R --vanilla -e "
if (requireNamespace('rmarkdown', quietly = TRUE)) {
  rmarkdown::render('README.Rmd')
  rmarkdown::render('index.Rmd')
  cat('Successfully rendered README.Rmd and index.Rmd\n')
} else {
  cat('rmarkdown not available - this will be handled by GitHub Actions\n')
}
"

echo "Badge update process completed!"
