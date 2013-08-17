#separate stars, galaxies, quasars

#you will probably need these later
library(ggplot2)
library(data.table)

#change this to your own local repo
#outside of all functions so will be global

root_dir="/Users/davej/TW-summit/"
data_dir=root_dir

#your functions below

read.astro.raw<-function(){
   #a simple reader for the astro data-set (SDSS Survey)
   file=paste(data_dir,'/','sdss.csv',sep='')
   #read the csv file into a data.frame
   data=read.csv(file)
   #make it a data.table object
   data=data.table(data)
   return(data)   
}

read.astro<-function(){
   #a simple reader for the astro data-set (SDSS Survey)
   file=paste(data_dir,'/','sdss.csv',sep='')
   #read the csv file into a data.frame
   data=read.csv(file)
   #convert to a data.table, a more powerful data structure
   data=data.table(data)
   #keep only rows with specClass = 1,2,3 which is star,galaxy,quasar
   data=data[specClass %in% c(1,2,3),]
   #replace specClass number with a 'factor' 
   spec.classes=factor(c('star','galaxy','quasar'))
   data[,objtype:=spec.classes[specClass]]
   #drop these columns
   data[,specClass:=NULL]
   data[,objid:=NULL]
   #function defined below
   add.colors(data)
   return(data)
}

add.colors<-function(data){
   #add the colors, note this is side effecting because data.table
   #is passed by reference unlike data.frame
   data[,ug:=u-g]
   data[,gr:=g-r]
   data[,ri:=r-i]
   data[,iz:=i-z]
}

score.my.classifyer<-function(predicted.class,true.class) {
   #calculate the precision, recall and F-score for 
   #predicted and true binary classifications
   
   #check to make sure there are logicals TRUE and FALSE
   stopifnot(class(predicted.class)=="logical")
   
}

test.logistic<-function(){
   num=300
   d1=data.table(x=rnorm(num),y=rnorm(num),cl="quasar")
   d2=data.table(x=rnorm(num)+2,y=rnorm(num)+2,cl="not_quasar")
   d=rbind(d1,d2)
   p<-ggplot(d,aes(x,y,colour=cl))+geom_point()
   print(p)
   mod=glm(class~x+y,data=d)
   print(mod)
}

test.log<-function(d=read.astro()){
   d[,quasar:=objtype == "quasar"]
   mod=glm(formula=quasar~ug+gr,data=d)   
   print(mod)
   d[,logit:=predict(mod,d)]
   d[,predictor:=exp(logit)/(1+exp(logit))]
   d[,cat:=predict(mod,d,type="response")]
   return(d)
}

