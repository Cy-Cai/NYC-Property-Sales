
# Creating training and testing set, 20% will assign as testing set, draw without replacement

set.seed(111)
test_index <-  createDataPartition(y=edx$SALE.PRICE,times=1,p=0.2,list=F)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure zip zode in validation set are also in edx set
test_set <- temp %>% 
    semi_join(train_set, by = "ZIP.CODE")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
#save the data files, it is used in the R markdownd file
save(train_set,file="rda/train_set.rda")
save(test_set,file="rda/test_set.rda")
rm(test_index,temp,removed)


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








