# trust examination
library(lmerTest)
library(brms)
library(psych)
library(effectsize)

# load data
load((file='../Vaccine_aligned.RData'))

# descriptive
describeBy(data.filtered$trust_6,group=data.filtered$residing_country)

# for each country
# see explanaory power

load('../Vaccine_H2.RData')

table.countries <- table(data.filtered$residing_country)
countries <- labels(table.countries)[[1]]
n.countries <- length(countries)

# test each
# frequentist
etas <- matrix(nrow=n.countries)
mean.trust <- matrix(nrow=n.countries)
sd.trust <- matrix(nrow=n.countries)
for (i in 1:n.countries){
  current <- data.filtered[data.filtered$residing_country == countries[[i]],]
  tryCatch( {current.lm <- lm(vx ~ trust_6+consp+gender + education + work_location + age+
                     SSS_faml+ relationship_status, data=current)
  current.eta<-eta_squared(current.lm)
  etas[i]<-current.eta[1,2]
  mean.trust[i] <- mean(current$trust_6, na.rm=T)
  sd.trust[i] <- sd(current$trust_6,na.rm=T)},
  error=function(cond) {
    etas[i]<-NA
    mean.trust[i]<-NA
    sd.trust[i]<-NA
  },
  warning=function(cond){
    etas[i]<-NA
    mean.trust[i]<-NA
    sd.trust[i]<-NA
  }
  )
}

corr.test(etas,mean.trust)
corr.test(etas,sd.trust)
sd.median <- median(sd.trust,na.rm = T)

data.filtered$sd <- -1
# high low
for (i in 1:n.countries){
  if (is.na(sd.trust[i])){
    
  }else{
  if (sd.trust[i]>=sd.median){
    data.filtered[data.filtered$residing_country == countries[i],'sd'] <- 1
  }else{
    data.filtered[data.filtered$residing_country == countries[i],'sd'] <- 0
  }}
}

# high
lm.high <- lmer(vx ~ trust_6+consp+gender + education + work_location + age+
                SSS_faml+ relationship_status + (1|residing_country), 
                data=data.filtered[data.filtered$sd == 1,])
# low
lm.low <- lmer(vx ~ trust_6+consp+gender + education + work_location + age+
                SSS_faml+ relationship_status + (1|residing_country), 
               data=data.filtered[data.filtered$sd == 0,])

