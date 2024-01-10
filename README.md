# Identification

* Title: Using Machine Learning to Separate Good and Bad Equity Mutual FundS: Evidence From Brazil

* Authors:

    * Pedro Teles

    * Robert Iquiapaza

    * Wanderci Bittencourt

* Date: March 2023

# Folder Contents

* 00_setup.R: Script to install and load all the required packages (latest  version).

* 01_clean_data.R: Takes the raw data and clean it. This cleaned data will be stored as .rds files in the folder clean_data.

* 02_prepare_model_data.R: Takes the clean data and uses it to build the features, and the dependent variable (abnormal return), for each fund and date. This file will output, to the model folder, a .csv file containing the features and abnormal returns. 

* 03_run_model.py: Based on the data prepared for modeling, run multiple models on a month-by-month base. This file will output a .csv file, to the model/predictions folder, containing the predictions and the execution time for each model.

* 04_build_portfolio.R: Analyze the predictions made by the models.

* 99_functions.R: Set of functions that are going to be used.

# Data Availability Statement

Due to storage costs, raw data is unavailable here but can be requested via email at pedroteles17@gmail.com.

```{r}

# Computational Requirements

R 4.2.1 [64-bit]:

The file "00_setup.R" will install all dependencies (latest version), and should be run once prior to running other programs .R. For more information, check the "Instructions" section. 

* tidyverse (2.0.0)
* readxl (1.4.2)
* lubridate (1.9.2)
* xts (0.12.1)
* PerformanceAnalytics (2.0.4)
* pacman (0.5.1)

Python 3.10.4:

* pandas (1.5.2)
* numpy (1.22.3)
* scikit-learn (1.1.1)
* lightgbm (3.3.2)
* xgboost (1.6.1)

The code was last run on a 4 core 11th Gen Intel Core i7-1165G7 laptop, with Ubuntu version 22.04, 16 GB of RAM, and 512GB of SSD. Computation took several hours.

# Instructions

First, make sure that you have R and Python installed and that you have the folder containing the R and Python codes as your working directory.

Then, the code should be run in the folowing order:

1- 00_setup.R

2- 01_clean_data.R

3- 02_prepare_model_data.R

4- 03_run_model.py

5- 04_build_portfolio.R

No further action is needed on the replicator's part.

# Data Qaulity Checks

## Manual Correction 1:

Some investment funds have a Net Asset Value (NAV) equal to 0 on their last day of operation before closing. Out of the 26 funds that have a NAV data point equal to 0, 25 of them have 0 as their last data point. However, there is one exception which is fund 451304. For this fund, the value of 0 is not its last data point, but rather the penultimate (second to last) one.

Implication: We set all zeros to missing.

## Manual Correction 2:

Fund 462934 had NAV of 0.0000001 on the antepenultimate date before closing (2021-11-11). Prior to that date, the value was 0.3574221, and after that date, the value was 0.3196338.

Implication: We set the NAV for 2021-11-11 to missing.

## Manual Correction 3:

Fund 468932 had NAV of 0.0000001 on the last date before closing (2021-11-11). Prior to that date, the value was 353.1366785.

Implication: We set the NAV for 2021-11-11 to missing.

## Manual Correction 4:

Fund 439649 NAV: 1541.903427 (2017-07-12); 1.904499 (2017-07-13); 1.899616 (2017-07-14); 1451.057852 (2017-07-17).

Implication: We set these values (2017-07-13 and 2017-07-14) to missing.

## Manual Correction 5:

Fund 286461 NAV: 1.0166165 (2014-08-29); 114.8804088 (2014-09-01); 0.9009222 (2014-09-02).

Implication: We set this value (2014-09-01) to missing.

## Manual Correction 6:

For Fund 216879, the Net Asset Value (NAV) series starts at a value of 1 on July 23rd, 2008. The next day, on July 24th, 2008, the NAV jumps significantly to a value of 28.18835.

Implication: We set this value (2008-07-23) to missing.

## Manual Correction 7:

For Fund 174718, the Net Asset Value (NAV) series starts at a value of 10 on 2006-12-06. The next day, the NAV jumps significantly to a value of 201.4266.

Implication: We set this value (2006-12-06) to missing.

## Manual Correction 8:

Fund 448087 NAV: 98.75633 (2017-12-07); 1818.18181 (2017-12-08). Fund closes after 2017-12-08.

Implication: We set this value (2017-12-08) to missing.

## Manual Correction 9:
For Fund 147354, the Net Asset Value (NAV) series starts at a value of 1 on 2005-07-08. The next trading day, the NAV jumps significantly to a value of 13.219489.

Implication: We set this value (2005-07-08) to missing.

## Manual Correction 10:
For Fund 168343, the Net Asset Value (NAV) series starts at a value of 1 on 2006-07-04. The next trading day, the NAV jumps significantly to a value of 11.73944.

Implication: We set this value (2006-07-04) to missing.

## Manual Correction 11:
For Fund 506583, the Net Asset Value (NAV) series starts at a value of 1 on 2019-09-02. The next trading day, the NAV jumps significantly to a value of 10.

Implication: We set this value (2019-09-02) to missing.

## Manual Correction 12:
For Fund 211966, the Net Asset Value (NAV) series starts at a value of 1 on 2008-06-20. The next trading day, the NAV jumps significantly to a value of 6.961836.

Implication: We set this value (2008-06-20) to missing.

## Manual Correction 13:
For Fund 177210, the Net Asset Value (NAV) series starts at a value of 1 on 2007-01-03. The next trading day, the NAV jumps significantly to a value of 5.657753.

Implication: We set this value (2007-01-03) to missing.