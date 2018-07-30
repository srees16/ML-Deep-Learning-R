#Thompson Sampling

dataset=read.csv('Ads_CTR_Optimisation.csv')

#Implementing Thomson sampling
N=10000
d=10
adsselected=integer(0)
numbersofrewards_1=integer(d)
numbersofrewards_0=integer(d)
totalreward=0
for(n in 1:N) {
  ad=0
  max_random=0
  for(i in 1:d) {
    randombeta=rbeta(n=1,shape1=numbersofrewards_1[i]+1,shape2=numbersofrewards_0[i]+1)
    if(randombeta>max_random) {
      max_random=randombeta
      ad=i
    }
  }
  adsselected=append(adsselected,ad)
  reward=dataset[n,ad]
  if(reward==1) {
    numbersofrewards_1[ad]=numbersofrewards_1[ad]+1
  } else {
    numbersofrewards_0[ad]=numbersofrewards_0[ad]+1
  }
  totalreward=totalreward+reward
}
hist(adsselected,col='blue',main = 'Histogram of Ads selections',xlab='Ads',ylab='No of times each ad was selected')