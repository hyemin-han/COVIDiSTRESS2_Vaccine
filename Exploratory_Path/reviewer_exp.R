# exploration
# 1. correlation

library(lmerTest)
library(brms)
library(EMAtools)
library(psych)
library(bayestestR)

# load aligned data
load((file='../Vaccine_H2.RData'))
load((file='../Vaccine_H3.RData'))

# correlation
corr.test(data.filtered[,c('trust_6','trust_7','consp','anti','vx','vaccine_0neutral')])

# simultaneous path model
prior.coef <- brms::prior(cauchy(0.,1),class='b')
model_mediator1 <- bf(trust_7~consp+anti + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+anti+consp|residing_country))
model_mediator2 <- bf(trust_6~consp+anti + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+anti+consp|residing_country))
model_outcome <- bf(vx ~ anti+consp+trust_6+trust_7 + gender + education + work_location + age+
                       SSS_faml+ relationship_status+ (1+trust_7+anti+trust_6+consp|residing_country))
med_result = brm(
  model_mediator1 +model_mediator2 + model_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef, iter=10000
)

hypothesis(med_result,'trust7_anti<0')
hypothesis(med_result,'trust6_consp<0')
hypothesis(med_result,'trust6_anti<0')
hypothesis(med_result,'trust7_consp<0')


hypothesis(med_result,'vx_anti<0')
hypothesis(med_result,'vx_consp<0')
hypothesis(med_result,'vx_trust_7>0')
hypothesis(med_result,'vx_trust_6>0')


# simple model
model_mediators1 <- bf(trust_7~anti + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+anti|residing_country))
model_mediators2 <- bf(trust_6~consp + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+consp|residing_country))
model_outcomes <- bf(vx ~ anti+consp+trust_6+trust_7 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_7+anti+trust_6+consp|residing_country))

med_results = brm(
  model_mediators1 +model_mediators2 + model_outcomes + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef, iter=10000
)

hypothesis(med_results,'trust7_anti<0')
hypothesis(med_results,'trust6_consp<0')
hypothesis(med_results,'vx_anti<0')
hypothesis(med_results,'vx_consp<0')
hypothesis(med_results,'vx_trust_7>0')
hypothesis(med_results,'vx_trust_6>0')

# compare
bf <- bayes_factor(med_results,med_result,log=T,maxiter=20000)

save (med_result,med_results,bf,file='additional.RData
      ')
