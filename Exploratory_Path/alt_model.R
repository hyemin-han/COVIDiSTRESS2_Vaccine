# alt model test

library(lmerTest)
library(brms)
library(EMAtools)
library(psych)
library(bayestestR)

# load aligned data
load((file='../Vaccine_H2.RData'))
load((file='../Vaccine_H3.RData'))


# Govt
# simultaneous path model
prior.coef <- brms::prior(cauchy(0.,1),class='b')
model_mediator1 <- bf(consp~trust_6 + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+trust_6 |residing_country))
model_outcome <- bf(vx ~ consp+trust_6+ gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_6+consp|residing_country))


# simultaneous path model (original)
model_mediator1o <- bf(trust_6~consp + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+consp|residing_country))

med_result = brm(
  model_mediator1  + model_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef, iter=4000
)

med_resulto = brm(
  model_mediator1o  + model_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef, iter=4000
)

# model compare
bf <- bayesfactor(med_result,med_resulto,maxiter=10000)
