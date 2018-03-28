days=data.frame(Plant=c('A','A','B','B','B','C'),Line=c(1,2,1,2,3,1),Month=c(37,37,37,37,37,37),Days=c(0,0,0,0,0,0),last='empty')
days
days$last<-as.character(days$last)

magic<-function(product,to_region){
  info<-production_capacity[production_capacity$Product==product & production_capacity$Capacity>0,c('Plant','Line','Capacity')]
  info$prod_cost<-0
  info$fixed<-0
  info$Delivery<-0
  info$ccost<-0
  info$cdays<-0
  for (i in 1:length(info[,1])){
    info$prod_cost[i]<-production_cost$Production_cost[production_cost$Product_ID==product & production_cost$Plant==info$Plant[i]]
    info$fixed[i]<-fixed$Cost[fixed$Plant==info$Plant[i]]
    info$Delivery[i]<-delivery$Delivery_cost[delivery$Plant==info$Plant[i] & delivery$Region==to_region]
    print(info)
    if(days$last[days$Plant==info$Plant[i] & days$Line==info$Line[i]]!=empty){
      print(info)
      info$ccost[i]<-change_cost$Cost[change_cost$Plant==info$Plant[i] & change_cost$Line==info$Line[i]]
      info$cdays[i]<-change_days$Days[change_days$To==product & change_days$From==days$last[days$Plant==info$Plant[i] & days$Line==info$Line[i]]]
      print(info)
    }
  }
  info$total<-info[,4]+info[,5]+info[,6]+info[,7]
  info<-info[order(info$total,-info$Capacity),]
  return(info)
}

for (i in 1:166){
  dem<-demand2$Demand[i]*0.65
  info<-magic(demand2$Product_ID[i],demand2$Region[i])
  j<-1
  while(dem>0){
    days_req<-dem/info$Capacity[j]+info$cdays[j]
    if(days_req>14){
      days_req<-days_req+1}
    if((30-days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line])>=days_req){
      days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line]<-days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line]+days_req
      days$last[days$Plant==info$Plant[j] & info$Line[j]==days$Line]<-demand2$Product_ID[i]
      dem<-0
      if(any(colnames(days)[colnames(days)==demand2$Product_ID[i]])){
        days[days$Plant==info$Plant[j] & info$Line[j]==days$Line,demand2$Product_ID[i]]<-days_req
      }
      else{
        days$new<-0
        colnames(days)[colnames(days)=='new']<-demand2$Product_ID[i]
        days[days$Plant==info$Plant[j] & info$Line[j]==days$Line,demand2$Product_ID[i]]<-days_req
      }
    }
    else{
      days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line]<-days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line]+days_req
      days$last[days$Plant==info$Plant[j] & info$Line[j]==days$Line]<-demand2$Product_ID[i]
      dem<-dem-info$Capacity[j]*(30-days$Days[days$Plant==info$Plant[j] & info$Line[j]==days$Line])
      j<-j+1
      if(any(colnames(days)[colnames(days)==demand2$Product_ID[i]])){
        days[days$Plant==info$Plant[j] & info$Line[j]==days$Line,demand2$Product_ID[i]]<-days_req
      }
      else{
        days$new<-0
        colnames(days)[colnames(days)=='new']<-demand2$Product_ID[i]
        days[days$Plant==info$Plant[j] & info$Line[j]==days$Line,demand2$Product_ID[i]]<-days_req
      }
    }
  }
}

