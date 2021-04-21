install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("stringr")
install.packages("DT")
install.packages("treemap")
install.packages("plotly")
install.packages("arules")
install.packages("arulesViz")
install.packages("visNetwork")
install.packages("igraph")
install.packages("kableExtra")

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(treemap)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library("RColorBrewer")
install.packages("devtools")
devtools::install_github("ropensci/plotly")


orders <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/orders.csv')
products <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/products.csv')
order_products <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/order_products__train.csv')
order_products_prior <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/order_products__prior.csv')
aisles <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/aisles.csv')
departments <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/departments.csv')


kable(head(orders,12))
kable(head(order_products,10))
kable(head(products,10))
kable(head(order_products_prior,10))

#recoding
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#When do customer order?

#---Hour of day---

orders %>%
  ggplot(aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count", fill = "black")
#most orders are between 8:00-20:00

#--Day of Week--
orders %>%
  ggplot(aes(x=order_dow)) +
  geom_histogram(stat = "count", fill = "blue")
#most orders are on weekends

#When do customers order again?
orders %>%
  ggplot(aes(x=days_since_prior_order))+
  geom_histogram(stat = "count", fill="red")
#People seem to order more often exactly 1 week

#Products that are sold most
tmp <- order_products %>%
  group_by(product_id) %>%
  summarize(count = n()) %>%
  top_n(20, wt = count) %>%
  left_join(select(products, product_id, product_name),by="product_id")%>%
  arrange(desc(count))
kable(tmp)

#Product sale count
tmp %>%
  ggplot(aes(x=reorder(product_name, -count),y=count))+
  geom_bar(stat="identity", fill="red")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.title.x = element_blank())
#Most often sold is Banana

#How often the customers order the same product again
tmp <- order_products %>%
  group_by(reordered) %>%
  summarize(count = n()) %>%
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)
#Result shows almost 59.85% products are reordered again

tmp %>%
  ggplot(aes(x=reordered, y=count, fill=reordered))+
  geom_bar(stat = "identity")

#Top 20 products that have highest probability of being reordered again
tmp <- order_products %>%
  group_by(product_id) %>%
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40)%>%
  top_n(20, wt=proportion_reordered) %>%
  arrange(desc(proportion_reordered))%>%
  left_join(products, by="product_id")
kable(tmp)

#Graphical representation of products reordered again
tmp %>%
  ggplot(aes(x=reorder(product_name, -proportion_reordered), y=proportion_reordered))+
  geom_bar(stat = "identity", fill="red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

#Items ppl put into their cart first
tmp <- order_products %>%
  group_by(product_id, add_to_cart_order)%>%
  summarize(count = n()) %>% mutate(pct=count/sum(count))%>%
  filter(add_to_cart_order == 1, count>20)%>%
  arrange(desc(pct))%>%
  left_join(products, by="product_id")%>%
  select(product_name, pct, count)%>%
  ungroup()%>%
  top_n(20, wt=pct)
kable(tmp)
#People seem to be quite certain about multifold towels

#Graph
tmp %>%
  ggplot(aes(x=reorder(product_name, -pct), y=pct))+
  geom_bar(stat = "identity", fill="pink")+
  theme(axis.text.x = element_text(angle=90, hjust=1), axis.title.x = element_blank())+coord_cartesian(ylim = c(0.4,0.7))
        
#Association between time of last order and probability of reorder
order_products %>%
  left_join(orders, by="order_id") %>%
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat = "identity", fill="orange")
#Same things are purchased more often in the same day and after a month customer move towards new products

#Association between number of orders and probability of reordering
order_products %>%
  group_by(product_id) %>%
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n, y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="green")+
  coord_cartesian(xlim=c(0,2000))
#Products with higher no of order are more likely to be ordered again
#As time passes the proportion of reordered item decreases

#Organic vs Non-organic
products <- products %>%
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name), 'organic'), "organic","not organic"), organic=as.factor(organic))
tmp <- order_products %>%
  left_join(products, by="product_id")%>%
  group_by(organic)%>%
  summarize(count=n())%>%
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>%
  ggplot(aes(x=organic, y=count, fill=organic))+
  geom_bar(stat = "identity")

#Reordering organic vs non-organic
tmp <- order_products %>%
  left_join(products, by = "product_id") %>%
  group_by(organic) %>%
  summarize(mean_reordered = mean(reordered))
kable(tmp)

tmp %>%
  ggplot(aes(x=organic, fill=organic, y=mean_reordered))+
  geom_bar(stat = "identity")

#Visualizing the product portfolio
#usage of treemap package to visualize the structure
tmp <- products %>%
  group_by(department_id, aisle_id) %>%
  summarize(n=n())
tmp <- tmp %>%
  left_join(departments, by="department_id")
tmp <- tmp %>%
  left_join(aisles, by="aisle_id")
tmp2 <- order_products %>%
  group_by(product_id) %>%
  summarize(count=n()) %>%
  left_join(products, by="product_id") %>%
  ungroup()%>%
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

treemap(tmp2, index = c("department", "aisle"),vSize = "onesize",vColor = "department", palette="Set3", title="", sortID="-sumcount",border.col="#FFFFFF", type = "categorical", fontsize.legend = 0, bg.labels = "#FFFFFF")

treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")

#Top 10 aisle that represent 45.3% of sale
tmp <- order_products %>%
  left_join(products, by="product_id") %>%
  left_join(aisles, by="aisle_id") %>%
  left_join(departments, by="department_id") %>%
  group_by(aisle,department) %>%
  tally(sort = TRUE) %>%
  mutate(perc = round(100*n/nrow(order_products),2))%>%
  ungroup()%>%
  top_n(20)
tmp %>%
  ggplot(aes(x=reorder(aisle, -n), y=n, fill = department))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())


#----Main algo begins here----
order_products_train <- fread('D:/Nithya_Official/Subjects/BE/Sem VIII/BDA/Miniproject/Dataset/order_products__train.csv')


#Create shopping basket
order_baskets = order_products_train %>%
  inner_join(products, by = "product_id") %>%
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

#create transaction data
transactions <- as(order_baskets$basket,"transactions")
inspect(transactions[1])         #Select no. serial no. of transaction
#Output shows that one transaction contains list of products

#Implementing Apriori algorithm
#Parameters Support=0.005, Confidence=0.25
#Among generate rules, there were some redundant rules so they were pruned. There were total 27 rules, after pruning. 
#The first rule tells that if customer buy organic cilantro, they are 6.21 times more likely to buy limes compared to random. 
#Top 5 rules are shown below rules

rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.25))

#Remove redundant rule
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table(lhs = labels(lhs(rules)),
                       rhs = labels(rhs(rules)),
                       quality(rules))[order(-lift),]
head(rules_dt, 5)

#Item Frequency Plot, it tells us how many times an item has occurred in our dataset as compared to others.

library("RColorBrewer")
par(mar = rep(2,4))
arules::itemFrequencyPlot(transactions,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency(Relative)")

#Scatter Plot
#Rules with high lift have typically a relatively low support:
plotly_arules(rules)

#Interactive Scatterplot

sel <- plot(rules, measure = c("support", "lift"),
            shading = "confidence",
            interactive = TRUE)
