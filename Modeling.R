if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")

head(train_set)
sapply(train_set, class)

#The simplest model: SALE.PRICE~GROSS.SQUARE.FEET
fit_simplest<- lm(train_set$SALE.PRICE~train_set$GROSS.SQUARE.FEET)
library(broom)
tidy(fit_simplest)
summary(fit_simplest)

prediction
                     