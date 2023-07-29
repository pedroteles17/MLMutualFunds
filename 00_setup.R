if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, xts, PerformanceAnalytics, stargazer, ggrepel, viridis)
`%ni%` <- Negate(`%in%`)
source('99_functions.R')
