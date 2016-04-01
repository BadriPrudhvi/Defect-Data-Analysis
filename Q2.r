setwd("E:\\project 2\\topic\\lucene")
files = dir(getwd(),pattern=".csv",recursive=TRUE)
call=load.data()
lucene.aic = compute.AIC(call)
load.data = function() {
  data.list = list()
  for (k in 1:20) {
    filename = paste('topic',k*5,'log.csv', sep='')
    data.list[[k]] = read.csv(filename, header = T, row.names = 1)
  }
  data.list
}
x = 5*(1:20)
plot(x, lucene.aic[,3], col = 'deeppink', type = 'l',lwd=2,main="Explanatory Power of Lucene",xlab = 'TOPICS',ylab= 'AIC Score')
lines(x,lucene.aic[,1], col = 'limegreen', type = 'l',lwd=2)
lines(x,lucene.aic[,2], col = 'coral4', type = 'l',lwd=2)
lines(x,lucene.aic[,4], col = 'firebrick2', type = 'l',lwd=2)
lines(x,lucene.aic[,5], col = 'blue', type = 'l',lwd=2)
legend("top",legend=c("hcm","loc","bf","Topic","Combine"),lty=1,lwd=2,col=c("deeppink","limegreen","coral4","firebrick2","blue"),ncol=2,
       bty="n",cex=0.8,text.col=c("deeppink","limegreen","coral4","firebrick2","blue"))

compute.AIC = function(data.list) {
  topic.aic = combine.aic = loc.aic = bf.aic = hcm.aic = rep(0, 20)
  for (i in 1:20) {
    topic.aic[i] = AIC.model(data.list[[i]], 'TOPIC')
    combine.aic[i] = AIC.model(data.list[[i]], 'COMBINE')
    loc.aic[i] = AIC(lm(bug ~ loc, data = data.list[[i]]))
    bf.aic[i] = AIC(lm(bug ~ bf, data = data.list[[i]]))
    hcm.aic[i] = AIC(lm(bug ~ hcm, data = data.list[[i]]))
  }
  min_loc = min(loc.aic)
  min_bf = min(bf.aic)
  min_hcm = min(hcm.aic)
  min_topic = min(topic.aic)
  min_combine = min(combine.aic)
  topic_k = which(topic.aic==min_topic)
  combine_k = which(combine.aic==min_combine)
  result = rep(0,7)
  result = cbind(min_loc,min_bf,min_hcm,topic_k,min_topic,combine_k,min_combine) 
  print(result)
  cbind(loc.aic,bf.aic,hcm.aic,topic.aic,combine.aic)
}



AIC.model = function(dat, predictor) {
  k = ncol(dat) - 4
  bug = dat[,k+1]
  if (predictor == 'TOPIC') {
    new.dat = dat[,1:k]
  }  
  
  if (predictor == 'COMBINE') {
    new.dat = dat[,-(k+1)]
  }  
  
  pca = princomp(new.dat)
  
  p = select.pca(pca, 0.90)
  
  new.dat = as.data.frame(pca$scores[,1:p])
  
  model = lm(bug ~ ., data = new.dat)
  
  AIC(model)
}

select.pca = function(pca, amount) {
  pca.var = pca$sdev^2
  sum.var = sum(pca.var)
  cum.sum = 0
  for (p in 1:length(pca.var)) {
    cum.sum = cum.sum + pca.var[p]
    if (cum.sum >= amount * sum.var)
      break
  }
  p
}

