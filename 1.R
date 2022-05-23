library(kernlab)
data(spam)

mean1 <-function(k,type="spam"){
  v <-as.numeric(k==type)
  return(mean(v))
}

do.peel <-function(data,dataname,target.va,alpha.peel=0.05){
  num <-nrow(data)
  peel.num <-ceiling(num*alpha.peel)
  new.frame <-data.frame()
  subdata<-data
  for (i in dataname){
    k <-sort(data[,i],decreasing=FALSE)
    k.min <-k[peel.num]
    k.max <-k[num-peel.num]
    subdata.min <-subset(subdata,subdata[,i]>=k.min)
    subdata.max <-subset(subdata,subdata[,i]<=k.max)
    if (mean1(subdata.min[,target.va])<=mean1(subdata.max[,target.va])){
      new.frame[1,i]=min(k)
      new.frame[2,i]=k.max
      new.frame[3,i]=mean1(subdata.max[,target.va])
    }
    else{
      new.frame[1,i]=k.min
      new.frame[2,i]=max(k)
      new.frame[3,i]=mean1(subdata.min[,target.va])
    }
  }
  n <-which.max(new.frame[3,])
  name <-colnames(new.frame)[n]
  return(append(new.frame[,name],name))
}

peel <-function(data,target.va,alpha.peel=0.05,beta=0.5){
  num <-nrow(data)
  process <-data.frame()
  dataname <-colnames(data[,-which(colnames(data)==target.va)])
  repeat{
    l <-do.peel(data,dataname,target.va,alpha.peel)
    process <-rbind(process,l)
    p.min <-as.numeric(l[1])
    p.max <-as.numeric(l[2])
    p.value <-as.numeric(l[3])
    p.name <-l[4]
    data <-data[which(data[,p.name]>=p.min&data[,p.name]<=p.max),]
    if (nrow(data)<=beta*num){
      break
    }
  }
  return(list(data,process))
}

do.paste <-function(data,dataname,data.peeled,target.va,alpha.paste=0.01){
  num <-nrow(data.peeled)
  num.paste <-floor(num*alpha.paste)
  newframe <-data.frame()
  data.n <-data
  for (i in dataname){
    data<-data.n
    for (j in dataname){
      if (j!=i){
        data<-data[which(data[,j]<=max(data.peeled[,j])&data[,j]>=min(data.peeled[,j])),]
      }
    }
    k <-sort(data[,i],decreasing = FALSE)
    p.min <-min(data.peeled[,i])
    p.max <-max(data.peeled[,i])
    n.min <-min(which(k==p.min))
    n.max <-max(which(k==p.max))
    kn.min <-n.min-num.paste
    kn.max <-n.max+num.paste
    if (kn.min <1&kn.max<=length(k)){
      newdata <-data[which(data[,i]>=min(data.peeled[,i])&data[,i]<=k[kn.max]),]
      newframe[1,i] <-min(data.peeled[,i])
      newframe[2,i]<-k[kn.max]
      newframe[3,i]<-mean1(newdata[,target.va])
    }
    else if (kn.min >=1&kn.max>length(k)){
      newdata <-data[which(data[,i]<=max(data.peeled[,i])&data[,i]>=k[kn.min]),]
      newframe[1,i] <-k[kn.min]
      newframe[2,i]<-max(data.peeled[,i])
      newframe[3,i]<-mean1(newdata[,target.va])
    }
    else if (kn.min>=1&kn.max<=length(k)){
      newdata1 <-data[which(data[,i]>=min(data.peeled[,i])&data[,i]<k[kn.max]),]
      newdata2 <-data[which(data[,i]<=max(data.peeled[,i])&data[,i]>k[kn.min]),]
      if (mean1(newdata1[,target.va])>=mean1(newdata2[,target.va])){
        newframe[1,i] <-min(data.peeled[,i])
        newframe[2,i]<-k[kn.max]
        newframe[3,i]<-mean1(newdata1[,target.va])
      }
      else{
        newframe[1,i] <-k[kn.min]
        newframe[2,i]<-max(data.peeled[,i])
        newframe[3,i]<-mean1(newdata2[,target.va])
      }
    }
  }
  n <-which.max(newframe[3,])
  name <-colnames(newframe)[n]
  return(append(newframe[,name],name))
}

past <-function(data,data.peeled,target.va,alpha.paste=0.01){
  process <-data.frame()
  data <-data
  value <-mean1(data.peeled[,target.va])
  dataname <-colnames(data.peeled[,-which(colnames(data.peeled)==target.va)])
  data.n <-data.peeled
  repeat{
    l <-do.paste(data,dataname,data.n,target.va,alpha.paste)
    l.v<-as.numeric(l[3])
    if (l.v>=value){
      value <-l.v
      process<-rbind(process,l)
      p.min <-as.numeric(l[1])
      p.max <-as.numeric(l[2])
      p.value <-as.numeric(l[3])
      p.name <-l[4]
      data.n<-data
      for (j in dataname){
        if (j!=p.name){
          data.n=data.n[which(data.n[,j]<=max(data.peeled[,j])&data.n[,j]>=min(data.peeled[,j])),]
        }
        else{
          data.n=data.n[which(data.n[,j]<=p.max&data.n[,j]>=p.min),]
        }
      }
      data.peeled <-data.n
    }
    else{
      break
    }
  }
  return(list(data.n,process))
}

prim <-function(data,target.va,set.va=NA,alpha.peel=0.05,alpha.paste=0.01,beta=0.5){
  target <-as.factor(data[,target.va])
  if (is.na(set.va)){
    newdata <-cbind(target,data[,-which(colnames(data)==target.va)])
    set.va <-colnames(data[,-which(colnames(data)==target.va)])
  }
  else{
    newdata <-cbind(target,data[,set.va])
  }
  target.va="target"
  p <-peel(newdata,target.va,alpha.peel,beta)
  data.peeled <-p[[1]]
  peel.process <-p[[2]]
  colnames(peel.process)<-c("min","max","value","variable")
  pa <-past(newdata,data.peeled,target.va,alpha.paste)
  data.pasted <-pa[[1]]
  paste.process <-pa[[2]]
  colnames(paste.process)<-c("min","max","value","variable")
  m <-list(data.pasted,peel.process,paste.process)
  return(m)
}



k <-prim(spam,"type",beta = 0.2)
