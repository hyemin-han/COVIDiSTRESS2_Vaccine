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

# mediation test
library(brms)

# prior -> cauchy distribution 
prior.coef <- brms::prior(cauchy(0.,1),class='b')

# mediation model
model_mediator <- bf(consp~trust_6 + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1+trust_6|residing_country))
model_outcome <- bf(vx ~ consp+trust_6 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_6+consp|residing_country))

med_result = brm(
  model_mediator + model_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)
options(width = 2000)
save(med_result, file = 'med_test.RData')
bayestestR::mediation(med_result)
