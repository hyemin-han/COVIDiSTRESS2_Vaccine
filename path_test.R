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
summary(med_result)
bayestestR::mediation(med_result)



# Next. trust _ 7 -> anti -> vx
# mediation model2
data.filtered$anti<-scale(data.filtered$anti)
model_mediator2 <- bf(anti~trust_7 + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1+trust_7|residing_country))
model_outcome2 <- bf(vx ~ anti+trust_7 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_7+anti|residing_country))
med_result2 = brm(
  model_mediator2 + model_outcome2 + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)
summary(med_result2)
bayestestR::mediation(med_result2)



# reverse test
model_mediator.r <- bf(trust_6~consp + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1+consp|residing_country))
model_outcome.r <- bf(vx ~ consp+trust_6 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_6+consp|residing_country))

med_result.r = brm(
  model_mediator.r + model_outcome.r + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)

summary(med_result.r)
bayestestR::mediation(med_result.r)

# compare two models
bf.comp.test <- bayes_factor(med_result,med_result.r)
bf.comp.test.r <- bayes_factor(med_result.r,med_result, maxiter=10000)

log(bf.comp.test.r$bf)


save.image(file = 'med_test.RData')