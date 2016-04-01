setwd("E:\\project 2\\topic\\Lucene")
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
plot(x, lucene.aic[,3], col = 'deeppink', type = 'l',lwd=2,main="Predictive Power of Lucene",xlab = 'TOPICS',ylab= 'Correlation',ylim=c(0.15,0.4))
lines(x,lucene.aic[,1], col = 'limegreen', type = 'l',lwd=2)
lines(x,lucene.aic[,2], col = 'coral4', type = 'l',lwd=2)
lines(x,lucene.aic[,4], col = 'firebrick2', type = 'l',lwd=2)
lines(x,lucene.aic[,5], col = 'blue', type = 'l',lwd=2)
legend("top",legend=c("hcm","loc","bf","Topic","Combine"),lty=1,lwd=2,col=c("deeppink","limegreen","coral4","firebrick2","blue"),ncol=2,
       bty="n",cex=0.8,text.col=c("deeppink","limegreen","coral4","firebrick2","blue"))

compute.AIC = function(data.list) {
  topic.aic = combine.aic = loc.aic = bf.aic = hcm.aic = rep(0, 20)
  for (i in 1:20) {
    dat = data.list[[i]]
    topic.aic[i] = AIC.model(dat[,-which(names(dat) %in% c("bug","loc","bf","hcm"))],dat$bug)
    combine.aic[i] = AIC.model(dat[,-which(names(dat) %in% c("bug"))],dat$bug)
    loc.aic[i] = lm.cval(bug ~ loc, dat)
    bf.aic[i] = lm.cval(bug ~ bf, dat)
    hcm.aic[i] = lm.cval(bug ~ hcm, dat)
  }
  max_loc = max(loc.aic)
  max_bf = max(bf.aic)
  max_hcm = max(hcm.aic)
  max_topic = max(topic.aic)
  max_combine = max(combine.aic)
  topic_k = which(topic.aic==max_topic)
  combine_k = which(combine.aic==max_combine)
  result = rep(0,7)
  result = cbind(max_loc,max_bf,max_hcm,topic_k,max_topic,combine_k,max_combine) 
  print(result)
  cbind(loc.aic,bf.aic,hcm.aic,topic.aic,combine.aic)
}




AIC.model = function(dat,bug) {
  pca = princomp(dat)
  p = select.pca(pca, 0.90)
  pca_s = as.data.frame(pca$scores[,1:p])
  pca_s$bug=bug
  model = lm.cval(bug ~ ., pca_s)
  model
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

lm.cval = function(form, dat, ratio = 0.5, iter = 50) {
  set.seed(7243)
  n = nrow(dat)
  cor = rep(0, iter)
  for (i in 1:iter) {
    trainset = sample.int(n, n*ratio)
    model = lm(form, data = dat[trainset,])
    predicted = predict(model, dat[-trainset,])
    actual = dat[-trainset,]$bug
    cor[i] = cor.test(predicted, actual, method = 'spearman')$estimate
  }
  cor
} 
