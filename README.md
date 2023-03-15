# MLMutualFunds
Code and data necessary to replicate the article "Using Machine Learning to Separate Good and Bad Equity Mutual Funds: Evidence from Brazil"

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