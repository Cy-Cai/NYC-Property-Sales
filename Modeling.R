if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")

head(train_set)
sapply(train_set, class)

#Define RMSE function
RMSE <- function(true_price,predicted_price){
  sqrt(mean((true_price-predicted_price)^2))
}

#The simplest model: SALE.PRICE~GROSS.SQUARE.FEET
fit_simplest<- lm(SALE.PRICE~GROSS.SQUARE.FEET,data = train_set)
library(broom)
tidy(fit_simplest)
summary(fit_simplest)

#save the model coefficient for the predcition calculation
b0<-fit_simplest$coefficients[1]
b1<-fit_simplest$coefficients[2]

#Performance
newd<-data.frame(GROSS.SQUARE.FEET=test_set$GROSS.SQUARE.FEET)
predicted_price<- predict(fit_simplest,newd,interval="prediction")
rmse_simplist_model<-RMSE(test_set$SALE.PRICE,predicted_price[,1])  



#Observe the RMSE by Property ID
#Create temp table to draw plot; calculate the residual by property 
temp_p<-test_set %>%mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET) %>% 
    mutate(residul=(SALE.PRICE-predicted_price)^2) %>%  group_by(Prop_ID) %>%
    summarise(r=sqrt(mean(residul)))
MED<-median(temp_p$r)

#Store the results 
rmse_results <- data.frame(method="gross sq ft",RMSE_in_thousand=round(rmse_simplist_model/1000,0),MED_in_thousand=round(median(temp_p$r)/1000,0))%>%
    mutate(k=round(RMSE_in_thousand/MED_in_thousand,0))

#Scatter Plot
temp_p  %>% 
    ggplot(aes(Prop_ID,r))+geom_point()

#Histogram
temp_p%>% filter(r<4*MED)%>%  ggplot(aes(r))+geom_histogram(bins=20)+
    labs(x="Residual",y="Property Count",title="Histogram Property Count vs Residual")+
    geom_vline(xintercept=median(temp_p$r))

#Observation: there are some prediction outliners; long tail 

#Observe the RMSE by Borough
#Create temp table to draw plot 
temp_p<-test_set %>%mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET) %>% 
    mutate(residul=(SALE.PRICE-predicted_price)^2) %>%  group_by(BOROUGH) %>%
    summarise(r=sqrt(mean(residul)))


#bar chart

temp_p%>% ggplot(aes(BOROUGH,r))+geom_bar(stat = "identity") +
    labs(x="Borough",y="Residual",title="Bar Chart Residual vs Borough")+
    geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))



