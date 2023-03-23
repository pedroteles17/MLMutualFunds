if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, xts, PerformanceAnalytics)
`%ni%` <- Negate(`%in%`)
source('99_functions.R')
