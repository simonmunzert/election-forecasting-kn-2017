
# install packages from CRAN
p_needed <- c("readr", # imports spreadsheet data
              "haven", # imports SPSS, Stata and SAS files
              "magrittr", #  for piping
              "plyr", # for consistent split-apply-combines
              "dplyr",  # provides data manipulating functions
              "devtools", # provides developer tools
              "stringr", # for string processing
              "lubridate", # dates and times
              "ggplot2", # for graphics
              "tidyr", # for tidying data frames
              "broom", # for tidying model output
              "janitor", # for basic data tidying and examinations
              "reshape2", # reshape data 
              "xtable", # generate table output
              "stargazer", # generate nice model table
              "rvest", # scraping suite
              "lme4", # linear mixed effects models
              "RCurl",
              "XML",
              "zoo",
              "ROAuth",
              "httpuv",
              "rtweet",
              "streamR", 
              "pageviews", 
              "statsgrokse", 
              "odds.converter"
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)