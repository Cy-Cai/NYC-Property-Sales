if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(graphics)) install.packages("graphics", repos = "http://cran.us.r-project.org")
if(!require(UsingR)) install.packages("UsingR", repos = "http://cran.us.r-project.org")



head(train_set)
sapply(train_set, class)

#Define RMSE function
RMSE <- function(true_price,predicted_price){
  sqrt(mean((true_price-predicted_price)^2))
}

#Exploratory Analysis



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

#Create temp table to draw plot 
temp_p<-test_set %>%mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET) %>% 
    mutate(residul=(SALE.PRICE-predicted_price)^2)

#Create a for loop to create bar chart for each variable

names(train_set)[c(2,3,4,5,9,12,18,19,20,21)]
loop.vector<-c(2,3,4,5,9,12,18,19,20)
# library(UsingR)
# par(mfrow=c(3,ceiling(length(loop.vector)/3)))
# par(mfrow=c(1,2))

loop.names<-names(temp_p)

for (i in loop.vector) {
 #Observe the RMSE by Borough
print(i)
    
 plot<-temp_p%>%dplyr::select(i,25) %>% 
     rename(A=1) %>%
        group_by(A) %>%
    summarise(r=sqrt(mean(residul)),n=NROW(A))%>%
        ggplot(aes(A,r,fill=n))+geom_bar(stat = "identity") +
       labs(x=loop.names[i],y="Residual")+scale_fill_gradient(low="blue", high="red")+
      geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
     geom_hline(yintercept=rmse_simplist_model)
        
 print(plot)
}

