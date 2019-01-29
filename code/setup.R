# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse',
              'skimr',
              "GGally",
              "stringr",
              "DataExplorer",
              "summarytools",
              "recipes",
              "magrittr",
              "corrr",
              "viridis",
              "h2o",
              "rsample",
              "h2o",
              "caret",
              "fs",
              "viridis")
library_install(packages)
