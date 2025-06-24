# Badge System Documentation

This document explains how the badge system works for the nowcaster package and how to maintain it.

## Overview

The nowcaster package uses badges to display:
- Package version (automatically pulled from DESCRIPTION)
- License information 
- Lifecycle stage (experimental)
- R-CMD-check status from GitHub Actions

## Badge Generation

Badges are generated using the `badger` R package in two R Markdown files:
- `README.Rmd` - generates badges for the GitHub repository README
- `index.Rmd` - generates badges for the pkgdown website

### Badge Types

1. **Version Badge**: `badger::badge_devel(color = "blue")`
   - Automatically reads version from DESCRIPTION file
   - Links to the GitHub repository

2. **License Badge**: `badger::badge_license(url = "https://github.com/covid19br/nowcaster/blob/main/LICENSE.md")`
   - Shows license type from DESCRIPTION
   - Links to LICENSE.md file

3. **Lifecycle Badge**: `badger::badge_lifecycle(stage = "experimental")`
   - Shows development stage
   - Links to r-lib lifecycle documentation

4. **R-CMD-check Badge**: Static GitHub Actions badge
   - Shows status of automated R package checks
   - Updates automatically when checks run

## Build Process

### GitHub Actions (Automated)

The `.github/workflows/pkgdown.yaml` workflow:
1. Installs necessary R packages (`badger`, `rmarkdown`)
2. Renders `README.Rmd` to update badges with current package info
3. Renders `index.Rmd` to update badges for the website
4. Builds the pkgdown site with updated badges
5. Deploys to GitHub Pages

### Local Testing

Run the `update_badges.sh` script to test badge generation locally:

```bash
./update_badges.sh
```

## Troubleshooting

### Badge not updating
- Ensure DESCRIPTION file has correct GitHub URL in the URL field
- Verify `badger` package is installed in build environment
- Check that R Markdown files are being rendered before site build

### Version showing as hardcoded
- Remove any `pkg = "package_name"` parameters from `badge_devel()` calls
- Ensure GitHub repository URL is in DESCRIPTION URL field

### Missing badges on website
- Verify `index.Rmd` contains all desired badge calls
- Check that pkgdown workflow includes R Markdown rendering steps
- Ensure `_pkgdown.yml` includes badges in strips configuration

## Maintenance

When updating package version:
1. Update version in DESCRIPTION file
2. Badges will automatically update on next website build
3. No manual changes needed to badge code

When adding new badges:
1. Add badge generation code to both `README.Rmd` and `index.Rmd`
2. Test locally with `update_badges.sh`
3. Commit changes - badges will update automatically on build
