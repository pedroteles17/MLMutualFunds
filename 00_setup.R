if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, xts, validate)
`%ni%` <- Negate(`%in%`)
