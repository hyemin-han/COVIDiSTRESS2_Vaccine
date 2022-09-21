# trust -> VHQB -> vaccine intent?

library(lmerTest)
library(brms)
library(EMAtools)

# load aligned data
load((file='Vaccine_aligned.RData'))

# standardize willingness and VAQB
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$consp <- scale(data.filtered$consp)
data.filtered$anti <- scale(data.filtered$anti)
data.filtered$trust_6 <- scale(data.filtered$trust_6)
data.filtered$trust_7 <- scale(data.filtered$trust_7)
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$vaccine_0neutral <- scale(data.filtered$vaccine_0neutral)

# bayesian model selection
# null mediation?
prior.coef <- brms::prior(cauchy(0.,1),class='b')
model0_mediator <- bf(vx~ gender + education + work_location + age+
                         SSS_faml+ relationship_status + (1|residing_country))
model0_outcome <- bf(vaccine_0neutral ~  gender + education + work_location + age+
                        SSS_faml+ relationship_status+ (1|residing_country))
options(width = 2000)
med0_result = brm(
  model0_mediator + model0_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)


# mediation without intercepts
model1_mediator <- bf(vx~trust_6+trust_7 + gender + education + work_location + age+
                         SSS_faml+ relationship_status + (1|residing_country))
model1_outcome <- bf(vaccine_0neutral ~ vx+trust_6+trust_7 + 
                       gender + education + work_location + age+
                        SSS_faml+ relationship_status+ (1|residing_country))
options(width = 2000)
med1_result = brm(
  model1_mediator + model1_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)
bayestestR::mediation(med1_result)

# mediation without random slopes
model2_mediator <- bf(vx~trust_6+trust_7 + gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1+trust_6+trust_7|residing_country))
model2_outcome <- bf(vaccine_0neutral ~ vx+trust_6+trust_7 + 
                       gender + education + work_location + age+
                       SSS_faml+ relationship_status+ 
                       (1+vx+trust_6+trust_7|residing_country))
options(width = 2000)
med2_result = brm(
  model2_mediator + model2_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)
bayestestR::mediation(med2_result)

# model comparison -> bayes factor
bf.med.10 <- bayes_factor(med1_result, med0_result) #INF
bf.med.20 <- bayes_factor(med2_result, med0_result) #INF
bf.med.21 <- bayes_factor(med2_result, med1_result, maxiter=10000, log=T,
                          cores=4) # 336.21748

save.image(file='additional_mediation.RData')

