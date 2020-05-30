if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(operator.tools)) install.packages("operator.tools", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(dplyr)
library(readr)
library(stringr)
library(operator.tools)
#Read data: This dataset is a record of every building or building unit (apartment, etc.) 
#old in the New York City property market over a 12-month period.

prop_sales <- data.frame(read_csv("data/nyc-rolling-sales.csv"))
prop_sales<- prop_sales %>% mutate(Prop_ID=paste(BOROUGH,BLOCK,LOT,sep="_"))
#replace '-' with zero in column GROSS.SQUARE.FEET and LAND.SQUARE.FEET and SALE.PRICE

prop_sales<-prop_sales %>% 
    transform(GROSS.SQUARE.FEET=as.numeric(str_replace_all(GROSS.SQUARE.FEET,"-","0")),
            LAND.SQUARE.FEET=as.numeric(str_replace_all(LAND.SQUARE.FEET,"-","0")),
            SALE.PRICE=as.numeric(str_replace_all(SALE.PRICE,"-","0")),
            ZIP.CODE=as.character(ZIP.CODE))
#filter out any SALE.PRICE lower than 50000
#there are still some transcation with  nonsensically small dollar amount
prop_sales<-prop_sales %>% 
    mutate(Building.Class=substr(BUILDING.CLASS.AT.TIME.OF.SALE,1,1)) %>% 
    filter(SALE.PRICE<=5000000 & SALE.PRICE>50000 & GROSS.SQUARE.FEET>0 & Building.Class %in% c("A") &TOTAL.UNITS==1)  #& BUILDING.CLASS.AT.TIME.OF.SALE %!in% c("A4","A3")

 prop_sales %>% group_by(TOTAL.UNITS) %>% summarise(NROW(Prop_ID))
# names(prop_sales)

prop_sales %>% filter(TOTAL.UNITS==1)

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding")

# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = prop_sales$SALE.PRICE, times = 1, p = 0.1, list = FALSE)
edx <- prop_sales[-test_index,]
temp <- prop_sales[test_index,]

# Make sure zip zode in validation set are also in edx set
validation <- temp %>% 
    semi_join(edx, by = "ZIP.CODE")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


#save as Rdata file
save(edx,file="rda/edx.rda")
save(validation,file="rda/validation.rda")

#remove temp variables
rm(prop_sales,test_index,temp,removed)
