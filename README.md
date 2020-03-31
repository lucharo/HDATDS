# HDATDS
Package with useful functions for the MSc HDA&amp;ML Translational Data Sciences and Computational Epidemiology projects

Functions built by March 31st:

* `BHSCalculator()`
* `factorBio()`
* `quantile_check()` --> mostly internal use

Install in your machine:
```
library(devtools)
library(dplyr)
devtools::install_github("lc5415/HDATDS")
library(HDATDS)
```

Example code:
```
# Original biomarker dataframe
data("bio.original_example")

# format names
bio = formatBio(bio.original, measurement = "first")
# alternatively
# data("bio.example")

# Load Covariates data frames (bio and cov) and merge them by ID
bio = merge(bio, cov[,c("ID","age_cl","gender")], by = "ID")
ids = bio$ID # keeping explicit copy of IDs
rownames(bio) = bio[,1] # assuming ID is the first column
bio = bio[,-1]
bio$age_cl = as.factor(bio$age_cl)
bio$gender = as.factor(bio$gender)

# Run BHS calculation using paper reference
scores_paper = BHSCalculator(bio, "Paper", stratified = T, bySystems = T)
```
