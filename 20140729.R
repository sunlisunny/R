data.read<-read.csv("201301IF.csv",header=F,sep=",")
time<-as.POSIXct(data.read$V1)
end<-data.read$V2
vol<-data.read$V3
inv<-data.read$V4
b1<-data.read$V5
b1v<-data.read$V6
s1<-data.read$V7
s1v<-data.read$V8
IF201306<-data.frame(time,end,vol,inv,b1,b1v,s1,s1v)
##IF201306[1:100,]
length(time)

##按天计算,要求第一条数据为整分钟
allday<-unique(time[as.numeric((time-time[1]))%%86400==0])
##allday
diff.i<-as.numeric(time-allday[1])
day.position<-(1:length(time))[diff.i%%86400==0]
for(i in 2:length(day.position))
if(time[day.position[i]]-time[day.position[i-1]]<1)
day.position[i]<-day.position[i-1]
day.position<-unique(day.position)
##day.position

day.min=day.max=day.end=day.start=0
for(i in 1:(length(allday)-1)){   
data.i<-IF201306[day.position[i]:(day.position[i+1]-1),]
day.start=c(day.start,data.i$end[1])
day.end=c(day.end,data.i$end[length(data.i$end)])
day.max=c(day.max,max(data.i$end))
day.min=c(day.min,min(data.i$end))
}
data.i<-IF201306[day.position[length(allday)]:length(vol),]
day.start<-c(day.start[2:length(allday)],data.i$end[1])
day.end<-c(day.end[2:length(allday)],data.i$end[length(data.i$end)])
day.max<-c(day.max[2:length(allday)],max(data.i$end))
day.min<-c(day.min[2:length(allday)],min(data.i$end))

day.change<-day.end-day.start
day.sign<-sign(day.change)
analysis.day<-data.frame(allday,day.start,day.end,day.max,day.min,day.change,day.sign)
##analysis.day



##按分钟计算,要求第一条数据为整分钟
allminute<-unique(time[as.numeric((time-time[1]))%%60==0])
##allminute

diff.i<-as.numeric(time-allminute[1])
minute.position<-(1:length(time))[diff.i%%60==0]
for(i in 2:length(minute.position))
if(time[minute.position[i]]-time[minute.position[i-1]]<1)
minute.position[i]<-minute.position[i-1]
minute.position<-unique(minute.position)
##minute.position


minute.min=minute.max=minute.end=minute.start=0
for(i in 1:(length(allminute)-1)){   
data.i<-IF201306[minute.position[i]:(minute.position[i+1]-1),]
minute.start=c(minute.start,data.i$end[1])
minute.end=c(minute.end,data.i$end[length(data.i$end)])
minute.max=c(minute.max,max(data.i$end))
minute.min=c(minute.min,min(data.i$end))
}
data.i<-IF201306[minute.position[length(allminute)]:length(vol),]
minute.start<-c(minute.start[2:length(allminute)],data.i$end[1])
minute.end<-c(minute.end[2:length(allminute)],data.i$end[length(data.i$end)])
minute.max<-c(minute.max[2:length(allminute)],max(data.i$end))
minute.min<-c(minute.min[2:length(allminute)],min(data.i$end))
minute.change<-minute.end-minute.start
minute.sign<-sign(minute.change)
analysis.minute<-data.frame(allminute,minute.start,minute.end,minute.max,minute.min,minute.change,minute.sign)
##analysis.minute[1:20,]




minute.need<-0
for(i in 2:length(minute.change)){
need.i<-0
if(minute.sign[i]+minute.sign[i-1]==2) if((minute.end[i]-minute.end[i-1])>2) need.i<-1   ##判断条件
if(minute.sign[i]+minute.sign[i-1]==-2) if((minute.end[i]-minute.end[i-1])<(-2)) need.i<-(-1)   ##判断条件
minute.need<-c(minute.need,need.i)
}
need[as.numeric(allminute-allminute[1])%%86400==0]<-0  ##9点15分，需要确认位置
need[as.numeric(allminute-allminute[137])%%86400==0]<-0  ##13点，需要确认位置
sum(abs(minute.need))

minute.real<-0
for(i in 3:length(minute.change)){
real.i<-0
if(abs(minute.need[i-1])==1) if(minute.need[i-1]*minute.sign[i]==1) real.i<-1   ##判断条件
minute.real<-c(minute.real,real.i)
}
minute.real<-c(minute.real,0)
sum(minute.real)


##analysis.minute[5816:5820,]



