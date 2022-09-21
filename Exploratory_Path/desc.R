# descriptive statistics
load((file='../Vaccine_aligned.RData'))
library(psych)
vars <- c('consp','anti','trust_6','trust_7','vx','vaccine_0neutral')

describe(data.filtered[,vars])
desc<-describeBy(data.filtered[,vars],group=data.filtered$residing_country)
countries <- length(desc)
tables <- matrix(nrow=countries,ncol=12)

for (i in 1:countries){
  
  current <- desc[[i]]
  for (j in 1:6){
    tables[i, ((2*j)-1)]<-current$mean[j]
    tables[i, ((2*j))]<-current$sd[j]
  }
}