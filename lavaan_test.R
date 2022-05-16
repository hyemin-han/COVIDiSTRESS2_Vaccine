# lavaan mlm test
library(lavaan)

load('Vaccine_aligned.RData')

# standardization
data.filtered$trust_6 <- scale(data.filtered$trust_6)
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$consp <- scale(data.filtered$consp)



# full
model.test <-
  '
  level: 1
    trust_6 ~ a1*consp
    vx ~ b1*consp + b2*trust_6
  level: 2
    trust_6 ~ consp
    vx ~ consp + trust_6
  
  ab := a1*b2
  total := b2 + (a1*b1)
'

# model specification
model.test <-
  '
  level: 1
    trust_6 ~ a1*consp
    vx ~ b1*consp + b2*trust_6
  level: 2
    trust_6 ~ 1
    vx ~ 1
  
  ab := a1*b2
  total := b2 + (a1*b1)
'

sem.test <- sem(model=model.test,data=data.filtered,cluster = 'residing_country',
                estimator='MLR')

summary(sem.test)
fitMeasures(sem.test)
lavInspect(sem.test,'h1')
