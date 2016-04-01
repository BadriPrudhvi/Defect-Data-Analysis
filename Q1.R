setwd("E:\\project 2\\topic\\lucene")

files = dir(getwd(),pattern=".csv",recursive=TRUE)
for (csvfile in files)
{ 
  read_file = read.csv(csvfile,header = TRUE, row.names=1, sep=",")
  if(csvfile=="topic10log.csv")
    one=correlation(read_file,10)
  else if(csvfile=="topic20log.csv")
    two=correlation(read_file,20)
  else if(csvfile=="topic50log.csv")
    three=correlation(read_file,50)
  else if(csvfile=="topic100log.csv")
    four=correlation(read_file,100)
}
correlation = function(file, value) {
  corr = rep(0, value)
  for (i in 1:value) 
    corr[i] = cor.test(file[,i], file$bug, method = 'spearman')$estimate
  corr
}
boxplot(one,two,three,four,
        ylab="Correlation of Bug",xlab="Topics Metrics",main="LUCENE",ylim=c(-0.2,0.28),
        col=c("cyan","green1","brown2","blue2"),
        names=c('10','20','50','100'))