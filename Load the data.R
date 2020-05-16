if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")


#Read data: This dataset is a record of every building or building unit (apartment, etc.) 
#old in the New York City property market over a 12-month period.
library(readr)
prop_sales <- data.frame(read_csv("data/nyc-rolling-sales.csv"))


# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding")

# if using R 3.5 or earlier, use `set.seed(1)` instead
library(caret)
test_index <- createDataPartition(y = prop_sales$SALE.PRICE, times = 1, p = 0.1, list = FALSE)
edx <- prop_sales[-test_index,]
validation <- prop_sales[test_index,]

#save as Rdata file
save(edx,file="rda/edx.rda")
save(validation,file="rda/validation.rda")

#remove temp variables
rm(prop_sales,test_index)
