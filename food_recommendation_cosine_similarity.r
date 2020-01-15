library(reshape2)
library(dplyr)
library(reshape)
library(plyr)
library(RPostgreSQL)
library(data.table)
library(stringdist)
library(stringr)
library(tidyverse)
library(rvest)
myRedshift <- src_postgres("zeta_reports",
                           host = "10.19.2.81",
                           port = 5439,
                           user = "ishitag",
                           password = "qPw5xx9n6QSsPE&r")


order_items <- tbl(myRedshift, sql("select a.businessid,a.storeid,a.counterid,a.userid,a.orderedat,a.receiptid,a.status,order_amount,itemid,categoryname,quantity,itemname
from quickshop.orders a
                                   join 
                                   quickshop.orderitems b
                                   on a.orderid = b.orderid
                                   where a.businessid in ('134975','151505','122449')
                                   and a.createdat > '2020-01-01 00:00:00+530'
                                   and status = 'DELIVERED'"))
df <- as.data.frame(order_items)
head(df)
df2 <- subset(df, select = c(7, 12, 13))
colnames(df2)

#get unique item names form the dataset and clustering
head(unique(as.character(df2$itemname)))
unique_items <- unique(as.character(df2$itemname))
typeof(unique_items)
distancemodels <- stringdistmatrix(unique_items,unique_items,method = "jw")
rownames(distancemodels) <- unique_items
hc <- hclust(as.dist(distancemodels))
dfClust <- data.frame(unique_items, cutree(hc, k=200))
names(dfClust) <- c('modelname','cluster')
plot(table(dfClust$cluster))
print(paste('Average number of models per cluster:', mean(table(dfClust$cluster))))
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head (dfClust[c('cluster','modelname')],100)





#getting combination of items and sort them alphabetically

head(combo_items_list,10)
df2%>%
  sort(df2$itemname)%>%
  paste(collapse = "-")
grouped <- df2 %>% sort(df2$itemname) %>% aggregate(itemname ~receiptid, data = df2, paste, collapse = ",")
combo_items_list <- grouped %>% filter(str_detect(itemname, ",")) 

combo_items_list_sorted <- sort(combo_items_list$itemname)

class(combo_items_list_sorted)



#checking different combos what we have got

combo_items_list_count <- as.data.frame(table(as.data.frame(combo_items_list_sorted)))
head(combo_items_list_count,2)
combo_items <- grouped[grouped$food_items %like% ";",c(4)]
rownames(combo_items) <- food_items
unique(combo_items$)

grouped_items_frequency <- (subset(grouped, select = c(1, 4)) %>% group_by(grouped$food_items) %>% summarise(count_of_combo = n_distinct(grouped$receiptid)))
a <- subset(grouped, select = c(1, 4))
a %>% group_by(grouped$food_items) %>% summarise(count_of_combo = n(grouped$receiptid))









####get user probabilities of ordering an item
b<-df %>% group_by(df$businessid,df$storeid,df$counterid) %>% dplyr::summarise(count = n())
class(b)
b

###get freuqncy of ordered items for ratings using cosine similarity


user_item_group_total <- df %>% group_by(itemname,userid) %>% dplyr::summarise(user_item_count = n()) %>% mutate(prob = user_item_count/nrow(df))
head(user_item_group_total)


arrange(user_item_group_total,prob,decreasing = TRUE)
ratings <- spread(user_item_group_total, itemname, prob, fill = 0, convert = FALSE, drop = TRUE,
       sep = NULL)
head(item_list)
item_list <- as.data.frame(unique(df$itemname))
colnames(item_list) <- c('item_list')

rating_matrix <- as.matrix(ratings)
cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
calc_cos_sim <- function(item_ordered, 
                         rating_mat = rating_matrix,
                         items = item_list,
                         return_n = 2) {
  # find our song
  item_col_index <- which(colnames(rating_matrix) == item_ordered)
  # calculate cosine similarity for each song based on 
  # number of plays for users
  # apply(..., 2) iterates over the columns of a matrix
  cos_sims <- apply(rating_matrix, 2,
                    FUN = function(y) 
                      cosine_sim(rating_matrix[,item_col_index], y))
  # return results
 data_frame(item_id = names(cos_sims), cos_sim = cos_sims) %>%
    filter(item_id != c(item_ordered,'user_item_count')) %>% merge(e , item_list , by.x = "item_id",by.y = "item_id") %>%
    arrange(desc(cos_sim.x)) %>%
    top_n(return_n, cos_sim.x) %>%
    select(item_id, cos_sim.x)
}

item_ordered <- 'Aloo Sandwich'
knitr::kable(calc_cos_sim('Ginger Tea Without Sugar'))




