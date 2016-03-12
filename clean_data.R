clean_data<-function(log,time=1:185,kmeans_k=6,user.all,brand_kmeans,cat_kmeans)
{

#筛选时间段内数据
  print("筛选时间段内数据")
  log<-log[log$time_stamp%in%time,]
  
  #添加购买次数
  print("添加购买次数")
  buy_freq<-data.frame(table(log$user_id[which(log$action_type==2)]))
  names(buy_freq)<-c("user_id","buy_freq")
  log_final<-merge(user.all,buy_freq,by.x = "user_id",by.y = "user_id",all.x = T)
  log_final$buy_freq[is.na(log_final$buy_freq)]<-0  
  #添加点击次数
  print("添加点击次数")
  click_freq<-data.frame(table(log$user_id[which(log$action_type==0)]))
  names(click_freq)<-c("user_id","click_freq")
  log_final<-merge(log_final,click_freq,by.x = "user_id",by.y = "user_id",all.x = T)
  log_final$click_freq[is.na(log_final$click_freq)]<-0 
  #添加购买点击比
  print("添加购买点击比")
  click_buy<-(log_final$click_freq+0.001)/(log_final$buy_freq+0.001)
  log_final<-data.frame(log_final,click_buy)
  #添加逛店次数
  print("添加逛店次数")
  ll<-unique(log[,c(1,4)])
  unum<-table(ll$user_id)
  sell_freq<-as.data.frame(unum)
  names(sell_freq)<-c("user_id","sell_freq")
  log_final<-merge(log_final,sell_freq,by.x = "user_id",by.y = "user_id",all.x = T)
  log_final$click_freq[is.na(log_final$sell_freq)]<-0 
  #添加购买逛店比
  print("添加购买逛店比")
  buy_sell<-(log_final$buy_freq)/(log_final$sell_freq)
  log_final<-data.frame(log_final,buy_sell)
#添加加入购物车次数
  print("添加加入购物车次数")
  cart_freq<-data.frame(table(log$user_id[which(log$action_type==3)]))
  names(cart_freq)<-c("user_id","cart_freq")
  log_final<-merge(log_final,cart_freq,by.x = "user_id",by.y = "user_id",all.x = T)
  log_final$cart_freq[is.na(log_final$cart_freq)]<-0  
  #添加加入购物车购买比
  print("添加加入购物车购买比")
  cart_buy<-(log_final$cart_freq+0.001)/(log_final$buy_freq+0.001)
  log_final<-data.frame(log_final,cart_buy)
  #点击加入购物车比
  print("点击加入购物车比")
  cart_click<-(log_final$click_freq+0.001)/(log_final$cart_freq+0.001)
  log_final<-data.frame(log_final,cart_click)
  
 #添加商品品牌总类
  print("添加商品品牌总类")
  brand_class<-brand_kmeans[match(log$brand_id,brand_kmeans$brand_id),2]
  cat_class<-cat_kmeans[match(log$cat_id,cat_kmeans$cat_id),2]
  log<-data.frame(log,brand_class,cat_class)
  log$brand_class<-as.factor( log$brand_class)
  log$cat_class<-as.factor( log$cat_class)
  user_in_time<-sort(unique(log$user_id))
  user_brand<-table(log$user_id,log$brand_class)
  user_brand<-user_brand/rowSums(user_brand)
  #print(ncol(user_brand))
  user_brand<-data.frame(user_in_time,user_brand[,1:ncol(user_brand)])
  log_final<-merge(log_final,user_brand,by.x = "user_id",by.y = "user_in_time",all.x = T)
 log_final[is.na(log_final)]<-0
 
 
#添加 商品种类
 print("添加 商品种类")
 user_cat<-table(log$user_id,log$cat_class)
 user_cat<-user_cat/rowSums(user_cat)
 user_cat<-data.frame(user_in_time,user_cat[,1:ncol(user_cat)])
 log_final<-merge(log_final,user_cat,by.x = "user_id",by.y = "user_in_time",all.x = T)
 log_final[is.na(log_final)]<-0
  resultlog<-log_final
  return(resultlog)
}

get_user<-function(log_train){
  print("获取所有用户ID")
  user.all<-as.data.frame(unique(log_train$user_id))
  names(user.all)<-"user_id"
  return(user.all)
}
merge_data<-function(data1,data2,data3,data4,data5,data6,data7)
{
final_log<-merge(data1,data2,by.x="user_id",by.y="user_id")
final_log<-merge(final_log,data3,by.x="user_id",by.y="user_id")
final_log<-merge(final_log,data4,by.x="user_id",by.y="user_id")
final_log<-merge(final_log,data5,by.x="user_id",by.y="user_id")
final_log<-merge(final_log,data6,by.x="user_id",by.y="user_id")
final_log<-merge(final_log,data7,by.x="user_id",by.y="user_id")
#final_log<-merge(final_log,data7,by.x="user_id",by.y="user_id")
return(final_log)
}

add_label<-function(x,user_info){
  print("添加训练label")
  class<-user_info[match(x$user_id,user_info[,1]),2]
  final_log<-data.frame(x[,2:ncol(x)],class)
  return(final_log)
}


my_process<-function(t1,t2,t3,t4,t5,t6,t7,log_train,info_train,brand_kmeans,cat_kmeans,add_label=T)
{
  train_t1<-clean_data(log_train,time = t1,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  train_t2<-clean_data(log_train,time = t2,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  train_t3<-clean_data(log_train,time = t3,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  train_t4<-clean_data(log_train,time = t4,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  train_t5<-clean_data(log_train,time = t5,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  train_t6<-clean_data(log_train,time = t6,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
 train_t7<-clean_data(log_train,time = t7,user.all = get_user(log_train),brand_kmeans = brand_kmeans,cat_kmeans = cat_kmeans)
  final_train<-merge_data(train_t1,train_t2,train_t3,train_t4,train_t5,train_t6,train_t7)
  if(add_label==T)
  {
  final_train<-add_label(final_train,info_train)
  final_train$class<-as.factor(final_train$class)
  }
  return (final_train);
  

}
getKmeans_brand<-function(log,k)
{
brand_class<-table(log$brand_id,log$class)/rowSums(table(log$brand_id,log$class))

brand_kmeans_k<-kmeans(brand_class,centers = k)

brand_id<-rownames(brand_class)
brand_kmeans_k<-data.frame(brand_id,brand_kmeans_k$cluster)
return (brand_kmeans_k)
                                               
}
getKmeans_cat<-function(log,k)
{
  
  cat_class<-table(log$cat_id,log$class)/rowSums(table(log$cat_id,log$class))
 
  cat_kmeans_k<-kmeans(cat_class,centers = k)
  cat_id<-rownames(cat_class)

  cat_kmeans_k<-data.frame(cat_id,cat_kmeans_k$cluster)
  return(cat_kmeans_k)
  
}


  
  