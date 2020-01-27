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
library(combinat)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
library(plyr)
install.packages("arulesViz", dependencies=TRUE)
library(arules)
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}
library(arulesViz)
myRedshift <- src_postgres("zeta_reports",
                           host = "10.19.2.81",
                           port = 5439,
                           user = "ishitag",
                           password = "qPw5xx9n6QSsPE&r")


c_order_items <- tbl(myRedshift, sql("select * from temp.items
                                   "))
order_items$combo_items <- as.character(order_items$combo_items)
df <- as.data.frame(order_items)
df %>% filter(is.na(df$receiptid))
df$ID <- seq.int(nrow(df))
df %>% filter(df$receiptid == 140066900656)

#dupkicate receipt ids to get combos
new_df <- data.frame(receiptid = NULL,combo = NULL)
df
for (i in c(1:nrow(df))){
  print(df$ID[i])
  receiptid <- as.character(df$receiptid[i])
  print(receiptid)
  item <- as.character((df$combo_items)[i])
  print(item)
  item_split <- strsplit((df$combo_items)[i],",")
  a <- as.matrix(combn(item_split[[1]],2))
  print(a)
  class(a)
#  print(ncol(a))
  for (j in (1: ncol(a))){
    new_df[nrow(new_df) + 1,'receiptid'] = receiptid
#    print(paste(a[1,1],',',a[2,1]))
    if (is.matrix(a) == TRUE){
      new_df[nrow(new_df) ,'combo'] = paste(a[1,j],',',a[2,j])
    }else 
    {
      new_df[nrow(new_df) ,'combo'] = paste(a[1],',',a[2])
    }
  }
  }

coalesce(nrow(df),as.integer(1))
item_split <- strsplit(as.character("Juice Rs.27 ,Bread Butter "),",")
item_split[[1]]

a <- combn(item_split[[1]],2)
a
paste(a[1,1],',',a[2,2])

a[2,1]
#get count of combos
combo_items_count <- df %>% group_by(df$combo_items) %>% tally()
combo_items_count %>% sort(n , decreasing = TRUE)

combo_items_count %>% strsplit(as.character(combo_items_count$`df$combo_items`[1]),',')
combo_items_count$`df$combo_items`[3]
i=0
df3 <- as.data.frame(NULL)
for (i in c(1:nrow(combo_items_count))){
  receiptid <- combo_items_count$
  e <- as.character((combo_items_count$`df$combo_items`)[i])
 f <- strsplit(e,',')
 print(f)
 print(length(f[[1]]))
  }
   



# sort by frequency of combos

count_of_combos <- new_df %>% group_by(new_df$combo) %>% dplyr::summarise(number = n())





#grouping of similar items
items<-NULL
items <- tbl(myRedshift, sql("select * from temp.items
                                   "))

item_data_frame <- as.character(item_data_frame)

kpm <- stringdistmatrix(item_data_frame$combo_items,useNames="strings",method="lv")
m = as.matrix(kpm)
item_data_frame <- as.data.frame(items)
item_data_frame$combo_items <- as.character(item_data_frame$combo_items)


kpm <- data.frame(as.matrix(kpm))
idx <- apply(kpm, 2, function(x) x >0 & x<4)
idx <- apply(idx, 1:2, function(x) if(isTRUE(x)) x<-1 else x<-NA)
final <- melt(idx) %>%
  filter(value==1) %>%
  select(Var1, Var2)
final[] <- lapply(final, as.character)
final <- final[!duplicated(data.frame(list(do.call(pmin,final),do.call(pmax,final)))),]
names(final) <- c('string 1', 'string 2')







#implemetation of apriori algo
head(c_order_items)
c_raw_df <- as.data.frame(c_order_items)
df_sorted <- c_raw_df[order(c_raw_df$receiptid),]
df_itemList <- ddply(c_raw_df,c("receiptid"), 
                     function(df1)paste(df1$combo_items, 
                                        collapse = ","))
df_itemList$receiptid <- NULL
colnames(df_itemList) <- c("itemList")
write.csv(df_itemList,"/Users/ishita.g/Documents/ItemList.csv", row.names = TRUE)

txn = read.transactions(file='/Users/ishita.g/Documents/ItemList.csv', rm.duplicates= TRUE, format="basket",sep=",",cols=1)
class(txn)
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.01,target="rules"))
inspect(basket_rules)
df_basket <- as(basket_rules,"data.frame")
plot(basket_rules)
itemFrequencyPlot(txn, topN = 5)
