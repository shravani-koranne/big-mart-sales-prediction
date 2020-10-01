library(data.table) # used for reading and manipulation of data 
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(corrplot)  # used for making correlation plot 
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 
getwd()='C:/Users/koran/Desktop/R/big mart sale prediction/salesprediction'
train = fread("train.txt")
test = fread("test.txt")
sample=fread("sample.txt")

dim(train)#looking at data set
dim(test)
summary(train)
summary(test)

test[,Item_Outlet_Sales := NA]
tail(test)
combi = rbind(train, test) # combining train and test datasets 
head(combi$Item_Outlet_Sales)
summary(combi)
summary(sample)

ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 1,fill="blue")+xlab("Item_Outlet_Sales")
p1=ggplot(combi)+geom_histogram(aes(combi$Item_Weight),binwidth = .05,fill="orange")+xlab("Item_weight")
p2=ggplot(combi)+geom_histogram(aes(combi$Item_Visibility),binwidth = .005,fill="orange")+xlab("Item_visibility")
p3=ggplot(combi)+geom_histogram(aes(combi$Item_MRP),binwidth = 1,fill="orange")+xlab("Item_MRP")
plot_grid(p1,p2,p3,nrow=1)#cowplot

ggplot(combi %>%
         group_by(Item_Fat_Content) %>% summarise(count=n()))
+geom_bar(aes(Item_Fat_Content,count),stat = "identity",fill="yellow")

combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"]="low fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="Low Fat"]="low fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"]="Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(count=n()))+
  geom_bar(aes(Item_Fat_Content,count),stat = "identity",fill="yellow2")

a=ggplot(combi %>% group_by(Item_Type) %>% summarise(count=n()))+
  geom_bar(aes(Item_Type,count),stat = "identity",fill="green") +xlab("") + 
  geom_label(aes(Item_Type, count, label = count), vjust = 0.10) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")

b=ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n()))+   
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") + 
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

c=ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(b, c, nrow = 1) 
plot_grid(a, second_row, ncol = 1)

d=ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1")+  
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(angle=45,hjust = 1))
# plot for Outlet_Type 
e=ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))
plot_grid(d,e,nrow=1)




#bivariate
train = combi[1:nrow(train)] # extracting train data from the combined data
g1=ggplot(train) +geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3)+      
              theme(axis.title = element_text(size = 8.5))
g2=ggplot(train) +geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3)+      
  theme(axis.title = element_text(size = 8.5))
g3=ggplot(train) +geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3)+      
  theme(axis.title = element_text(size = 8.5))
s1=plot_grid(g1,g2, ncol = 2)
plot_grid(g3,s1,nrow = 2)





p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

p13 = ggplot(train) +       geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

p14 = ggplot(train) +       geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

s2=plot_grid(p12,p13,ncol = 2)
plot_grid(p14,s2,nrow=2)



library(mice)
combi = rbind(train, test)
tail(combi)
md.pattern(combi)
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){    item = combi$Item_Identifier[i] 
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }

sum(is.na(combi$Item_Weight))
ggplot(combi)+geom_histogram(aes(combi$Item_Visibility),binwidth = .005,fill="orange")+xlab("Item_visibility")

zero_index = which(combi$Item_Visibility==0)
for(i in zero_index){    item = combi$Item_Identifier[i] 
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T) }
ggplot(combi)+geom_histogram(aes(combi$Item_Visibility),bins = 100,fill="grey")+xlab("Item_visibility")

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
#create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))
table(combi$Item_Type_new, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]


combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 

# years of operation for outlets 
combi[,Outlet_Years := 2018 - Outlet_Establishment_Year] 

combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 

# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]



# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",                                   
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",                                         
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,                                 
                                 ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,                                         
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)




combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]


num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with =  F]
prep_num = preProcess(combi_numeric, method=c("center", "scale")) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset
cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

set.seed(1234)
my_control = trainControl(method="cv", number=5)
linear_reg_mod = train(x = train[,-c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control)
print("5- fold cross validation scores:")
print(round(linear_reg_mod$resample$RMSE, 2))

sample$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(sample, "Linear_Reg_submit.csv", row.names = F)

