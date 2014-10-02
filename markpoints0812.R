## install.packages("quantmod")
library(quantmod)

## read file
data.minute.read<-read.csv("201303IF.analysis.minute.csv",header=T)
data.minute<-data.minute.read[,-1]

##par(mflow=c(2,1))
## detemine





data<-data.minute$minute.end
plot(data,type="l")


markpoints <- function(data, threshold, color) {
  m <- 10
  pos <- 1
  trend<-sign(data[m]-data[1])
  
  while(trend==0){
    m<-m+1
    trend<-sign(data[m]-data[1])
  }
  extre<-trend*max(trend*data[1:m])
  
  for(i in (m+1):length(data) ){
    extre.new <- trend * max(trend*data[last(pos):i])
    if(extre.new!=extre){
      extre<-extre.new
      ## find the extreme point 
      ## if exists more than 1 , then find the last one
      extre.po<-max((last(pos):i)[data[last(pos):i]==extre])
    }
    ## 比较当前值减变化量的绝对值处于最高点是否大于幅度阈值K
    if(abs(data[i]-extre)>threshold*extre){ ##
      pos<-c(pos,extre.po)
      trend<-(-1)*trend
    }
  }
  points(pos,data[pos],col=color)  
  return(pos)
}

pos.b <- markpoints(data, 0.002, "blue")
pos.r <- markpoints(data, 0.004, "red")


pre<-100  ##??ǰ??????
pos.pre<-pos.r[pos.r>100]
rpoint.diff<-diff(data[pos.r])#????֮???ļ۲?
rpoint.diff<-rpoint.diff[pos.r>100]#????100֮???ĵ?
rpoint.diff<-rpoint.diff[c(-length(rpoint.diff))]#ȥ????????һ??????why?
number.blue<-rep(NA,length(pos.pre))
range.pre<-rep(NA,length(pos.pre))
number.blue.in.red<-rep(NA,length(pos.pre))
number.red<-rep(NA,length(pos.pre))
slope<-rep(NA,length(pos.pre))
max.datapre<-rep(NA,length(pos.pre))
min.datapre<-rep(NA,length(pos.pre))
adj.r.squared<-rep(NA,length(pos.pre))
bpoint.mean<-rep(NA,length(pos.pre))
for(i in 1:length(rpoint.diff)){
  data.pre<-data[(pos.pre[i]-pre):(pos.pre[i]-1)]
  range.pre[i]<-max(data.pre)-min(data.pre)
  number.blue[i]<-sum(as.numeric(pos.b<pos.pre[i]&pos.b>pos.pre[i]-pre-1))#num of blue in pre minutes
  bpoint.diff<-diff(data[as.numeric(pos.b<pos.pre[i]&pos.b>pos.pre[i]-pre-1)])
  bpoint.mean[i]<-mean(bpoint.diff)
  #compute number of bule points between two red points
  number.blue.in.red[i]<-sum(as.numeric(pos.b[pos.b<pos.pre[i+1]]>pos.pre[i]-1))#num of blue in these 2 red
  number.red[i]<-sum(as.numeric(pos.r[pos.r<pos.pre[i]]>pos.pre[i]-pre-1))#num of red in pre minutes
  max.datapre[i]<-max(data.pre)-data[pos.pre[i]]
  min.datapre[i]<-min(data.pre)-data[pos.pre[i]]
  fit<-lm(data.pre~c(1:pre))
  slope[i]<-as.numeric(fit$coefficients[2])
  adj.r.squared[i]<-summary(fit)$adj.r.squared
  plot(c(1:pre),data.pre)
  abline(fit)
  
}
pre.red.timedif<-diff(pos.r)

need<-rep(NA,length(pos.pre))
for(i in 1:(length(pos.pre)-1)){
  need[i]<-0
  if(abs(rpoint.diff[i])>10) 	##ɸѡ????price.dif>10
    need[i]<-1
}
need[length(need)]<-0

#output<-data.frame(number.blue=number.blue[need==1],range.pre=range.pre[need==1],number.blue.in.red=number.blue.in.red[need==1],number.red=number.red[need==1],pre.red.timedif=pre.red.timedif[need==1],slope=slope[need==1],max.datapre=max.datapre[need==1],min.datapre=min.datapre[need==1])

#pos.pre[need==1]
write.csv(output,file="markpoints.csv")

#hist(number.red[need==1])
hist(adj.r.squared[need==1])
