# Run this script on a new machine/server to install required R packages
# for posts/eds222-final/index.qmd
#
# Usage: Rscript posts/eds222-final/setup.R

install.packages(c(
  "ggdag",
  "dagitty",
  "tidyverse",
  "sf",
  "terra",
  "here",
  "pscl",
  "yaml",
  "patchwork"
), repos = "https://cloud.r-project.org")
