
# Creating training and testing set, 20% will assign as testing set, draw without replacement

set.seed(111)
test_index <-  createDataPartition(y=edx$SALE.PRICE,times=1,p=0.2,list=F)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
#save the data files, it is used in the R markdownd file
save(train_set,file="rda/train_set.rda")
save(test_set,file="rda/test_set.rda")
rm(test_index)


#Explore the data
#Distribution of X1
#bar chart
train_set %>% group_by(X1) %>% count() %>% 
    ggplot(aes(x=X1,y=n)) +
    geom_bar(stat="identity", fill="red")

#scaterplot 

train_set%>%filter(BOROUGH==1) %>% 
    ggplot(aes(x=X1,y=SALE.DATE,shape=as.factor(BOROUGH)))+geom_point()

# X1 Colume seems a random number within each BOROUGH

#Distribution of Borough
#bar chart
train_set %>% group_by(BOROUGH) %>% count() %>% 
    ggplot(aes(x=BOROUGH,y=n)) +
    geom_bar(stat="identity", fill="red")

#


train_set %>%  ggplot(aes(log(SALE.PRICE)))+geom_histogram(bins=50)+
    labs(x="Residual",y="Property Count",title="Histogram Property Count vs Residual")








