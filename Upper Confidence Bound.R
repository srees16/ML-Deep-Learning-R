#Upper Confidence Bound
dataset=read.csv('Ads_CTR_Optimisation.csv')
N=10000
d=10
ads_selected=integer()
numbersofselections=integer(d)
sumsofrewards=integer(d)
totalreward=0
for(n in 1:N) {
  ad=0
  max_upperbound=0
  for(i in 1:d) {
    if(numbersofselections[i]>0) {
      averagereward=sumsofrewards[i]/numbersofselections[i]
      delta_i=sqrt(3/2*log(n)/numbersofselections[i])
      upperbound=averagereward+delta_i
    } else {
      upperbound=1e400
    }
    if(upperbound>max_upperbound) {
      max_upperbound=upperbound
      ad=i
    }
  }
  ads_selected=append(ads_selected,ad)
  numbersofselections[ad]=numbersofselections[ad]+1
  reward=dataset[n,ad]
  sumsofrewards[ad]=sumsofrewards[ad]+reward
  totalreward=totalreward+reward
}
#Visualizing the results - Histogram
hist(ads_selected,col='blue',main='Histogram of Ads selections',xlab = 'Ads',ylab = 'No of Times ad was selected')