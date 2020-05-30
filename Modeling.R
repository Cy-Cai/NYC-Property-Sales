if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(graphics)) install.packages("graphics", repos = "http://cran.us.r-project.org")
if(!require(UsingR)) install.packages("UsingR", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
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
library(broom)
library(graphics)
library(UsingR)
library(forcats)
load("rda/train_set.rda")
load("rda/test_set.rda")
head(train_set)
sapply(train_set, class)

#Define RMSE function
RMSE <- function(true_price,predicted_price){
  sqrt(mean((true_price-predicted_price)^2))
}

# AbEorrPer <- function(true_price,predicted_price){
#      mean(abs(true_price-predicted_price)/true_price)
#      }

#Exploratory Analysis
#The simplest model: SALE.PRICE~GROSS.SQUARE.FEET
fit_simplest<- lm(SALE.PRICE~GROSS.SQUARE.FEET,data = train_set)

tidy(fit_simplest)
summary(fit_simplest)

#save the model coefficient for the predcition calculation
b0<-fit_simplest$coefficients[1]
b1<-fit_simplest$coefficients[2]

#Performance
newd<-data.frame(GROSS.SQUARE.FEET=test_set$GROSS.SQUARE.FEET)
predicted_price<- predict(fit_simplest,newd,interval="prediction")
rmse_simplest_model<-RMSE(test_set$SALE.PRICE,predicted_price[,1])  

#Observe the RMSE by Property ID
#Create temp table to draw plot; calculate the residual by property 
temp_p<-test_set %>%mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET) %>% 
    mutate(residul=(SALE.PRICE-predicted_price)^2) %>%  group_by(Prop_ID) %>%
    summarise(r=sqrt(mean(residul)))
MED<-median(temp_p$r)

#Store the results 
rmse_results <- data.frame(method="gross sq ft",
                           RMSE_in_thousand=round(rmse_simplest_model/1000,0),
                           MED_in_thousand=round(median(temp_p$r)/1000,0), 
                           k=round(rmse_simplest_model/median(temp_p$r),0),
                           Adj_r_sq=summary(fit_simplest)[9])

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

# for (i in loop.vector) {
#  #Observe the RMSE by Borough
# print(i)
# 
#  plot<-temp_p%>%dplyr::select(i,26) %>%
#      rename(A=1) %>%
#         group_by(A) %>%
#     summarise(r=sqrt(mean(residul)),n=NROW(A))%>%
#         ggplot(aes(A,r,fill=n))+geom_bar(stat = "identity") +
#        labs(x=loop.names[i],y="Residual")+scale_fill_gradient(low="blue", high="red")+
#       geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#      geom_hline(yintercept=rmse_simplist_model)
# 
#  print(plot)
# }

#The 2nd Model: SALE.PRICE~GROSS.SQUARE.FEET+ IS.Manhattan
#y=b0+b1*x1+b2*(I(Manhattan)*x1)

#adding a column: if it is in Manhattan, it equals to Gross Square Feet; otherwise 0
temp <- train_set %>% mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                                       TRUE ~ 0     )*GROSS.SQUARE.FEET) 
fit_IsManhattan<- lm(SALE.PRICE~GROSS.SQUARE.FEET+GSFandManhattan,data = temp)

tidy(fit_IsManhattan)
summary(fit_IsManhattan)    

#save the model coefficient for the predcition calculation
b0<-fit_IsManhattan$coefficients[1]
b1<-fit_IsManhattan$coefficients[2]
b2<-fit_IsManhattan$coefficients[3]


#Performance
newd<-test_set %>% 
    mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                     TRUE ~ 0     )*GROSS.SQUARE.FEET) %>% 
    dplyr::select(GROSS.SQUARE.FEET,GSFandManhattan) 


predicted_price<- predict(fit_IsManhattan,newd,interval="prediction")
rmse_IsManhattan_model<-RMSE(test_set$SALE.PRICE,predicted_price[,1])  
## aberrorper_simplist_model<-AbEorrPer(test_set$SALE.PRICE,predicted_price[,1])

#Observe the RMSE by Property ID
#Create temp table to draw plot; calculate the residual by property 
temp_p<-test_set %>%
    mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                     TRUE ~ 0     )*GROSS.SQUARE.FEET) %>% 
    mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET+b2*GSFandManhattan) %>% 
    mutate(residul=(SALE.PRICE-predicted_price)^2) %>%
    mutate(r=sqrt(residul))

MED<-median(temp_p$r)

#Store the results 
rmse_results <-rbind(rmse_results, 
                     data.frame(method="Is Manhattan",
                                RMSE_in_thousand=round(rmse_IsManhattan_model/1000,0),
                                MED_in_thousand=round(median(temp_p$r)/1000,0), 
                                k=round(rmse_IsManhattan_model/median(temp_p$r),0),
                                Adj_r_sq=summary(fit_IsManhattan)[9]))

#Histogram
temp_p%>% filter(r<4*MED)%>%  ggplot(aes(r))+geom_histogram(bins=20)+
    labs(x="Residual",y="Property Count",title="Histogram Property Count vs Residual")+
    geom_vline(xintercept=median(temp_p$r))

#Observation: there are some prediction outliners; long tail 


#Create a for loop to create bar chart for each variable

names(train_set)[c(2,3,4,5,9,12,15,18,19,20,21)]
loop.vector<-c(2,3,4,5,9,12,15,18,19,20,24)
# library(UsingR)
# par(mfrow=c(3,ceiling(length(loop.vector)/3)))
# par(mfrow=c(1,2))

# loop.names<-names(temp_p)
# len<-ncol(temp_p)
# for (i in loop.vector) {
#     #Observe the RMSE by Borough
#     print(i)
#     plot<-temp_p%>%dplyr::select(i,len-1) %>% 
#         rename(A=1) %>%
#         group_by(A) %>%
#         summarise(r=sqrt(mean(residul)),n=NROW(A))%>%
#         ggplot(aes(A,r,fill=n))+geom_bar(stat = "identity") +
#         labs(x=loop.names[i],y="Residual")+scale_fill_gradient(low="blue", high="red")+
#         geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#         geom_hline(yintercept=rmse_IsManhattan_model)
#     
#     print(plot)
# }
# 
# #Observation: RMSE decrease, but still big; mainly impacted by 
# #transacation in Manhattan need to look deeper in to Manhattan's transcations
# 
# train_set %>%  filter(SALE.PRICE>=1000000) %>% 
#           ggplot(aes(x=Prop_ID,y=SALE.PRICE,color=factor(BOROUGH)))+
#     geom_point()
# 
# train_set %>%  filter(SALE.PRICE>=1000000) %>% 
#     group_by(BOROUGH) %>% 
#     summarise(NROW(SALE.PRICE))
# 
# train_set %>% filter(SALE.PRICE<1000000) %>% 
#     ggplot(aes(x=factor(BOROUGH),y=SALE.PRICE))+
#     geom_boxplot(outlier.colour="black", outlier.shape=16,
#                                                     outlier.size=2, notch=FALSE)
# #SALE.PRICE in Manhathan varies a lot. Need to determine extra factor for
# #Manhattan. We should look what factor is affecting the price other than 
# #the Borough
# 
# #let's look at the million dollars houses
# MillionProperty<-train_set %>% filter(SALE.PRICE>=1000000)
# 
# #let's look at bulding class category
# MillionProperty %>% group_by(BUILDING.CLASS.CATEGORY) %>% 
#     summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
#     arrange(desc(m))
# #let's look at the building class
# MillionProperty  %>% 
#     group_by(Building.Class) %>% 
#     summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
#     arrange(desc(m)) %>% filter(n>10,m>1000000)
# 
# #let's look at the detailded build class
# MillionProperty  %>% 
#   group_by(BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
#   summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
#   arrange(desc(m)) %>% filter(n>10,m>1000000)
# 
# #let's look at the total units
# MillionProperty  %>% 
#   group_by(TOTAL.UNITS) %>% 
#   summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE),s=sd(SALE.PRICE)) %>% 
#   arrange(desc(m)) %>% filter(n>10,m>1000000)
# 
# #the price for A4 type is twice more than the others
# #let's look at the distribution of A4 type by BOROUGH
# MillionProperty %>% filter(BUILDING.CLASS.AT.TIME.OF.SALE=="A4") %>% 
#                 group_by(BOROUGH) %>% 
#                 summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
#   arrange(desc(m)) %>% filter(n>10,m>1000000)

i=12
d<-temp_p%>%dplyr::select(i,len-1) %>% 
  rename(A=1) %>%
  group_by(A) %>%
  summarise(r=sqrt(mean(residul)),n=NROW(A))
sum(d$n[d$r>300000])/sum(d$n)

#Observation: there are A4 Type in both BOROUGH 1 and 3, but the A4 Type
#in Manhattan is 4 times of the others

MillionProperty %>% filter(BOROUGH=="1") %>% 
  group_by(BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
  summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
  arrange(desc(m)) %>% filter(n>10,m>1000000)


MillionProperty %>% filter(BOROUGH=="3") %>% 
  group_by(BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
  summarise(n=NROW(SALE.PRICE),m=mean(SALE.PRICE)) %>% 
  arrange(desc(m)) %>% filter(n>10,m>1000000)


# price per square feet varies in Manhattan. What is the driver of the differece?
train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  ggplot(aes(x=factor(BOROUGH),y=price_per_square_feet))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)

train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  ggplot(aes(x=factor(BUILDING.CLASS.AT.TIME.OF.SALE),y=price_per_square_feet))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)


train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  ggplot(aes(x=fct_reorder(NEIGHBORHOOD,price_per_square_feet,.desc=TRUE),y=price_per_square_feet))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)

train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  group_by(ZIP.CODE) %>% 
  summarise(m=mean(price_per_square_feet),s=sd(price_per_square_feet),n=NROW(Prop_ID)) %>% 
  arrange(desc(m))

train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  ggplot(aes(x=fct_reorder(ZIP.CODE,price_per_square_feet,.desc=TRUE),y=price_per_square_feet,fill=as.factor(BOROUGH)))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom")+
  labs(x="ZIP Code",y="Price Per Square Feet",fill="Borough")


#MOdel 3:  SALE.PRICE~GROSS.SQUARE.FEET+ IS.HIGH.PRICING.AREA
#y=b0+b1*x1+b2*(I(MANHATTAN))+b3*(I(HIGH.PRICING.AREA))


#adding a column: if it is in Manhattan, it equals to Gross Square Feet; otherwise 0
#adding a column: if it is in Manhattan, it equals to Gross Square Feet; otherwise 0

#find out the best price point to define High and Low
df<-data.frame()
for (p in seq(100,1000,by=50)) {
HighPricingArea<- train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  group_by(ZIP.CODE) %>% 
  summarise(m=mean(price_per_square_feet),n=NROW(Prop_ID)) %>% 
  arrange(desc(m)) %>% 
  mutate(PPSF=case_when(m>p~1,
                                        TRUE ~ 0))
temp <- train_set %>% 
                   left_join(HighPricingArea,by=c("ZIP.CODE")) %>% 
  mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                   TRUE ~ 0     )*GROSS.SQUARE.FEET,
         GSFandHIGH.PRICING.AREA=PPSF*GROSS.SQUARE.FEET)
  
fit_IsManhattanAndHighPricingArea<- lm(SALE.PRICE~GROSS.SQUARE.FEET+GSFandManhattan+ GSFandHIGH.PRICING.AREA,data = temp)

tidy(fit_IsManhattanAndHighPricingArea)
summary(fit_IsManhattanAndHighPricingArea)    
df<-rbind(df,c(p=p,R=summary(fit_IsManhattanAndHighPricingArea)[9] ) )
}
df[which.max(df$R.adj.r.squared),]
df %>% ggplot(aes(x=p,y=R.adj.r.squared))+geom_point()+geom_line()
# the best cut off is 450

HighPricingArea<- train_set %>% mutate(price_per_square_feet=SALE.PRICE/GROSS.SQUARE.FEET) %>% 
  group_by(ZIP.CODE) %>% 
  summarise(m=mean(price_per_square_feet),n=NROW(Prop_ID)) %>% 
  arrange(desc(m)) %>% 
  mutate(PPSF=case_when(m>450~1,
                        TRUE ~ 0))
temp <- train_set %>% 
  left_join(HighPricingArea,by=c("ZIP.CODE")) %>% 
  mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                   TRUE ~ 0     )*GROSS.SQUARE.FEET,
         GSFandHIGH.PRICING.AREA=PPSF*GROSS.SQUARE.FEET)



fit_IsManhattanAndHighPricingArea<- lm(SALE.PRICE~GROSS.SQUARE.FEET+GSFandManhattan+ GSFandHIGH.PRICING.AREA,data = temp)

tidy(fit_IsManhattanAndHighPricingArea)
summary(fit_IsManhattanAndHighPricingArea)[9]

#save the model coefficient for the predcition calculation
b0<-fit_IsManhattanAndHighPricingArea$coefficients[1]
b1<-fit_IsManhattanAndHighPricingArea$coefficients[2]
b2<-fit_IsManhattanAndHighPricingArea$coefficients[3]
b3<-fit_IsManhattanAndHighPricingArea$coefficients[4]

#Performance
newd<-test_set %>% 
  left_join(HighPricingArea,by=c("ZIP.CODE")) %>% 
  mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                   TRUE ~ 0     )*GROSS.SQUARE.FEET,
         GSFandHIGH.PRICING.AREA=PPSF*GROSS.SQUARE.FEET) %>%  
  dplyr::select(GROSS.SQUARE.FEET,GSFandManhattan,GSFandHIGH.PRICING.AREA) 


predicted_price<- predict(fit_IsManhattanAndHighPricingArea,newd,interval="prediction")
rmse_IsManhattanAndHighPricingArea_model<-RMSE(test_set$SALE.PRICE,predicted_price[,1])  
## aberrorper_simplist_model<-AbEorrPer(test_set$SALE.PRICE,predicted_price[,1])

#Observe the RMSE by Property ID
#Create temp table to draw plot; calculate the residual by property 
temp_p<-test_set %>%
  left_join(HighPricingArea,by=c("ZIP.CODE")) %>% 
  mutate(GSFandManhattan=case_when(BOROUGH=="1"~ 1,
                                   TRUE ~ 0     )*GROSS.SQUARE.FEET,
         GSFandHIGH.PRICING.AREA=PPSF*GROSS.SQUARE.FEET) %>%  
  mutate(predicted_price=b0+b1*GROSS.SQUARE.FEET+b2*GSFandManhattan+b3*GSFandHIGH.PRICING.AREA) %>% 
  mutate(residul=(SALE.PRICE-predicted_price)^2) %>%
  mutate(r=sqrt(residul))

MED<-median(temp_p$r)

#Store the results 
rmse_results <-rbind(rmse_results, 
                     data.frame(method="Is Manhattan and High Pricing Area",
                                RMSE_in_thousand=round(rmse_IsManhattanAndHighPricingArea_model/1000,0),
                                MED_in_thousand=round(median(temp_p$r)/1000,0), 
                                k=round(rmse_IsManhattanAndHighPricingArea_model/median(temp_p$r),0),
                                Adj_r_sq=summary(fit_IsManhattanAndHighPricingArea)[9]))


#Histogram
temp_p%>% filter(r<4*MED)%>%  ggplot(aes(r))+geom_histogram(bins=20)+
  labs(x="Residual",y="Property Count",title="Histogram Property Count vs Residual")+
  geom_vline(xintercept=median(temp_p$r))

#Observation: there are some prediction outliners; long tail 


#Create a for loop to create bar chart for each variable

names(train_set)[c(2,3,4,5,9,12,15,18,19,20,21)]
loop.vector<-c(2,3,4,5,9,12,15,18,19,20,24)
# library(UsingR)
# par(mfrow=c(3,ceiling(length(loop.vector)/3)))
# par(mfrow=c(1,2))

loop.names<-names(temp_p)
len<-ncol(temp_p)
for (i in loop.vector) {
  #Observe the RMSE by Borough
  print(i)
  plot<-temp_p%>%dplyr::select(i,len-1) %>% 
    rename(A=1) %>%
    group_by(A) %>%
    summarise(r=sqrt(mean(residul)),n=NROW(A))%>%
    ggplot(aes(A,r,fill=n))+geom_bar(stat = "identity") +
    labs(x=loop.names[i],y="Residual")+scale_fill_gradient(low="blue", high="red")+
    geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_hline(yintercept=rmse_IsManhattan_model)
  
  print(plot)
}

i=12
d<-temp_p%>%dplyr::select(i,len-1) %>% 
  rename(A=1) %>%
  group_by(A) %>%
  summarise(r=sqrt(mean(residul)),n=NROW(A))
sum(d$n[d$r>300000])/sum(d$n)


#
loop.names<-names(temp_p)
len<-ncol(temp_p)
i=18
temp_p%>%dplyr::select(i,len-1) %>% 
  rename(A=1) %>%
  group_by(A) %>%
  summarise(r=sqrt(mean(residul)),n=NROW(A))%>%
  ggplot(aes(A,r,fill=n))+geom_bar(stat = "identity") +
  labs(x=loop.names[i],y="Residual")+scale_fill_gradient(low="blue", high="red")+
  geom_text(aes(label=round(r,3)),size=3,check_overlap = TRUE,position=position_dodge(width=0.9), vjust=-0.25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=rmse_IsManhattan_model)

d<-temp_p%>%dplyr::select(i,len-1) %>% 
  rename(A=1) %>%
  group_by(A) %>%
  summarise(r=sqrt(mean(residul)),n=NROW(A))
sum(d$n[d$r>300000])
