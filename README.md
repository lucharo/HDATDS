# HDATDS
![](https://www.travis-ci.org/lc5415/HDATDS.svg?branch=master)

Package with useful functions for the MSc HDA&amp;ML Translational Data Sciences and Computational Epidemiology projects

Functions built by March 31st:

* `BHSCalculator()`
* `formatBio()`
* `quantile_check()` --> mostly internal use
* `Analysis()`: performs cross-validation for any given model and given dataset or combination of datasets

__Install in your machine (from RStudio):__
_Note:_ the imperial HPC RStudio session cannot install xgboost which is a dependency of this package, so __do not try to install this package from the HPC RStudio.__
```
library(devtools)
library(dplyr)
devtools::install_github("lc5415/HDATDS")
library(HDATDS)
```

__Install in your machine (Terminal):__
I would recommend installing this package in a new environment given dependecy issues may arise otherwise.
```
conda create --name NewProjectEnv
conda activate NewProjectEnv
conda install -c lucha6 r-hdatds
```

__Example code:__
```
# Original biomarker dataframe
data("bio.example")
data("cov")
# Load biomarkers and covariates data frames (bio and cov) and merge them by ID
bio = merge(bio.example, cov[,c("ID","age_cl","gender")], by = "ID")
ids = bio$ID # keeping explicit copy of IDs
rownames(bio) = bio[,1] # assuming ID is the first column
bio = bio[,-1]
bio$age_cl = as.factor(bio$age_cl)
bio$gender = as.factor(bio$gender)

bio = bio[complete.cases(bio), ] #function does not handle NAs internally

# Run BHS calculation using paper reference
scores_paper = BHSCalculator(bio, "Paper", stratified = TRUE, bySystems = TRUE)
```
