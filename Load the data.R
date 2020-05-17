if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")


#Read data: This dataset is a record of every building or building unit (apartment, etc.) 
#old in the New York City property market over a 12-month period.
library(readr)
prop_sales <- data.frame(read_csv("data/nyc-rolling-sales.csv"))
#replace '-' with zero in column GROSS.SQUARE.FEET and LAND.SQUARE.FEET and SALE.PRICE
library(dplyr)
library(stringr)
prop_sales<-prop_sales %>% 
    transform(GROSS.SQUARE.FEET=as.numeric(str_replace_all(GROSS.SQUARE.FEET,"-","0")),
            LAND.SQUARE.FEET=as.numeric(str_replace_all(LAND.SQUARE.FEET,"-","0")),
            SALE.PRICE=as.numeric(str_replace_all(SALE.PRICE,"-","0")))
#filter out any SALE.PRICE lower than 50000
#there are still some transcation with  nonsensically small dollar amount
prop_sales<-prop_sales %>% filter(SALE.PRICE>50000)

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
