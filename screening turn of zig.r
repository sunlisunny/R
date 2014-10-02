##加载quantmod
##读取文件
data.minute.read<-read.csv("201303IF.analysis.minute.csv",header=T)
data.minute<-data.minute.read[,-1]

##par(mflow=c(2,1))
##判断


data<-data.minute$minute.Close
plot(data,type="l")
m<-10
k<-0.002    ##蓝点的涨跌幅
po.b<-1
trend<-sign(data[m]-data[1])
while(trend==0){
m<-m+1
trend<-sign(data[m]-data[1])
}
extre<-trend*max(trend*data[1:m])

for(i in (m+1):length(data) ){
extre.new<-trend*max(trend*data[last(po.b):i])
if(extre.new!=extre){
extre<-extre.new
extre.po<-max((last(po.b):i)[data[last(po.b):i]==extre])
}
if((-1)*trend*(data[i]-extre)>k*extre){ ##
po.b<-c(po.b,extre.po)
trend<-(-1)*trend
}
}
points(po.b,data[po.b],col="blue")

k<-0.004    ##红点的涨跌幅
po.r<-1
trend<-sign(data[m]-data[1])
while(trend==0){
m<-m+1
trend<-sign(data[m]-data[1])
}
extre<-trend*max(trend*data[1:m])

for(i in (m+1):length(data) ){
extre.new<-trend*max(trend*data[last(po.r):i])
if(extre.new!=extre){
extre<-extre.new
extre.po<-max((last(po.r):i)[data[last(po.r):i]==extre])
}
if((-1)*trend*(data[i]-extre)>k*extre){ ##
po.r<-c(po.r,extre.po)
trend<-(-1)*trend
}
}
points(po.r,data[po.r],col="red")



##筛选1,不用了
##pre<-100
##diff<-data[po.r[2:length(po.r)]]-data[po.r[1:(length(po.r)-1)]]
##po.need<-po.r[abs(diff)>10]
##po.need<-po.need[po.need>pre]
##number.blue<-NA
##range.need<-NA
##for(i in 1:length(po.need)){
##data.need<-data[(po.need[i]-pre):(po.need[i]-1)]
##range.need<-c(range.need,max(data.need)-min(data.need))
##number.blue<-c(number.blue,sum(as.numeric(po.b[po.b<po.need[i]]>(po.need[i]-pre-1))))
##}
##number.blue<-number.blue[c(-1)]
##range.need<-range.need[c(-1)]
##number.blue
##range.need
##hist(number.blue,breaks=seq(4.9,20.9,1))
##hist(range.need,breaks=seq(4.9,90,3))
##po.need

##plot(data,type="l")



##筛选2
pre<-100	##向前多少天
po.pre<-po.r[po.r>100]
rpoint.diff<-diff(data[po.r])
rpoint.diff<-rpoint.diff[po.r>100]
rpoint.diff<-rpoint.diff[c(-length(rpoint.diff))]
number.blue<-rep(NA,length(po.pre))
range.pre<-rep(NA,length(po.pre))
for(i in 1:length(po.pre)){
data.pre<-data[(po.pre[i]-pre):(po.pre[i]-1)]
range.pre[i]<-max(data.pre)-min(data.pre)
number.blue[i]<-sum(as.numeric(po.b[po.b<po.pre[i]]>po.pre[i]-pre-1))
}

need<-rep(NA,length(po.pre))
for(i in 1:(length(po.pre)-1)){
need[i]<-0
if(abs(rpoint.diff[i])>10) 	##筛选条件
need[i]<-1
}
need[length(need)]<-0

po.pre[need==1]
number.blue[need==1]
range.pre[need==1]
##hist(number.blue,breaks=seq(0.5,20.5,1))
##hist(range.pre,breaks=seq(5.5,99.5,2))



##vol<-data.minute$minute.Volume[1:3500]
##plot(vol,type="h")


