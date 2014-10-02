##加载quantmod

##读取文件
data.read<-read.csv("201004IF.csv",header=F,sep=",")
time<-as.POSIXct(data.read$V1)
end<-data.read$V2
vol<-data.read$V3
inv<-data.read$V4
b1<-data.read$V5
b1v<-data.read$V6
s1<-data.read$V7
s1v<-data.read$V8
data<-data.frame(time,end,vol,inv,b1,b1v,s1,s1v)

##按天处理,
time.start<-as.POSIXct("2010-01-01 06:00:00")
diff.i<-difftime(time,time.start,unit="days")
day.diff<-unique(floor(diff.i))
day.position<-rep(NA,length(day.diff))
for(i in 1:length(day.diff))
day.position[i]<-min((1:length(end))[diff.i>=day.diff[i]])
allday<-time[day.position]

day.Low=day.High=day.Close=day.Open=day.Volume=rep(NA,length(day.position))
for(i in 1:(length(day.position)-1)){   
data.i<-data[day.position[i]:(day.position[i+1]-1),]
day.Open[i]<-first(data.i$end)
day.Close[i]<-last(data.i$end)
day.High[i]<-max(data.i$end)
day.Low[i]<-min(data.i$end)
day.Volume[i]<-last(data.i$vol)
}
data.i<-data[day.position[length(day.position)]:length(end),]
day.Open[length(day.position)]<-first(data.i$end)
day.Close[length(day.position)]<-last(data.i$end)
day.High[length(day.position)]<-max(data.i$end)
day.Low[length(day.position)]<-min(data.i$end)
day.Volume[length(day.position)]<-last(data.i$vol)
day.change<-day.Close-day.Open
day.sign<-sign(day.change)
analysis.day<-data.frame(allday,day.position,day.Open,day.Close,day.High,day.Low,day.Volume,day.change,day.sign)

write.csv(analysis.day,file="analysis.day.csv")

##按分钟计算
diff.i<-difftime(time,time.start,unit="mins")
minute.diff<-unique(floor(diff.i))
minute.position<-rep(NA,length(minute.diff))
for(i in 1:length(minute.diff))
minute.position[i]<-min((1:length(end))[diff.i>=minute.diff[i]])
allminute<-time[minute.position]

minute.Low=minute.High=minute.Close=minute.Open=minute.Volume.cul=rep(NA,length(minute.position))
for(i in 1:(length(minute.position)-1)){   
data.i<-data[minute.position[i]:(minute.position[i+1]-1),]
minute.Open[i]<-first(data.i$end)
minute.Close[i]<-last(data.i$end)
minute.High[i]<-max(data.i$end)
minute.Low[i]<-min(data.i$end)
minute.Volume.cul[i]<-last(data.i$vol)
}
data.i<-data[minute.position[length(minute.position)]:length(end),]
minute.Open[length(minute.position)]<-first(data.i$end)
minute.Close[length(minute.position)]<-last(data.i$end)
minute.High[length(minute.position)]<-max(data.i$end)
minute.Low[length(minute.position)]<-min(data.i$end)
minute.Volume.cul[length(minute.position)]<-last(data.i$vol)
minute.Volume<-c(first(minute.Volume.cul),minute.Volume.cul[2:length(allminute)]-minute.Volume.cul[1:(length(allminute)-1)])
minute.Volume[minute.Volume<0]<-minute.Volume.cul[minute.Volume<0]
minute.change<-minute.Close-minute.Open
minute.sign<-sign(minute.change)
analysis.minute<-data.frame(allminute,minute.position,minute.Open,minute.Close,minute.High,minute.Low,minute.Volume,minute.change,minute.sign)

write.csv(analysis.minute,file="analysis.minute.csv")

