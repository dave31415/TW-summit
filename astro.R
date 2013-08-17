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
