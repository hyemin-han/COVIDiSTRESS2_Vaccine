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

# frequentist

# demographics to be determined....
# Age, gender, marital status, education level, SES, cohabitants, working from home

# H2a trust_6 ~ consp

options(width=1000)
# model 0 (null model) demo and random intercepts
h2a.0 <- lmer(trust_6 ~ gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h2a.1 <- lmer(trust_6 ~ consp+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h2a.2 <- lmer(trust_6 ~ consp+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1+consp|residing_country), data=data.filtered)


# Bayesian
# prior -> normal distribution 
prior.coef <- brms::prior(cauchy(0.,1),class='b')

#model 0
b.h2a.0 <- brms::brm(trust_6 ~ gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                           data=data.filtered, family = gaussian(),
                           cores=4,chains=4, save_pars = save_pars(all = T),
                           sample_prior ='yes', seed=1660415)

#model 1
b.h2a.1 <- brms::brm(trust_6 ~ consp+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h2a.2 <- brms::brm(trust_6 ~ consp+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1+consp|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

b.h2a.2

# model bayes factors
bf.h2a.1 <- bayes_factor(b.h2a.1,b.h2a.0)
bf.h2a.2 <- bayes_factor(b.h2a.2,b.h2a.0)
bf.h2a.21 <- bayes_factor(b.h2a.2,b.h2a.1)

log(as.numeric(bf.h2a.1[1]))
log(as.numeric(bf.h2a.2[1]))
log(as.numeric(bf.h2a.21[1]))



# predictor bayes factor check
# one direction -> positive
hypothesis(b.h2a.2,'consp<0')


# effect size
EMAtools::lme.dscore(h2a.1, type='lme4', data=data.filtered)



#####
# H2b : vx ~ trust _ 6
# model 0 (null model) demo and random intercepts
h2b.0 <- lmer(vx ~ gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h2b.1 <- lmer(vx ~ trust_6+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h2b.2 <- lmer(vx ~ trust_6+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1+trust_6|residing_country), data=data.filtered)



#model 0
b.h2b.0 <- brms::brm(vx ~ gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415)

#model 1
b.h2b.1 <- brms::brm(vx ~ trust_6+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h2b.2 <- brms::brm(vx ~ trust_6+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1+trust_6|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

b.h2b.2

# model bayes factors
bf.h2b.1 <- bayes_factor(b.h2b.1,b.h2b.0)
bf.h2b.2 <- bayes_factor(b.h2b.2,b.h2b.0)
bf.h2b.21 <- bayes_factor(b.h2b.2,b.h2b.1)

log(as.numeric(bf.h2b.1[1]))
log(as.numeric(bf.h2b.2[1]))
log(as.numeric(bf.h2b.21[1]))

hypothesis(b.h2b.2,'trust_6>0')

EMAtools::lme.dscore(h2b.1, type='lme4', data=data.filtered)

###
# H2c : vx ~ consp
# model 0 (null model) demo and random intercepts
h2c.0 <- lmer(vx ~ gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h2c.1 <- lmer(vx ~ consp+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h2c.2 <- lmer(vx ~ consp+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1+consp|residing_country), data=data.filtered)


#model 0
b.h2c.0 <- brms::brm(vx ~ gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415)

#model 1
b.h2c.1 <- brms::brm(vx ~ consp+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h2c.2 <- brms::brm(vx ~ consp+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1+consp|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

b.h2c.2

# model bayes factors
bf.h2c.1 <- bayes_factor(b.h2c.1,b.h2c.0)
bf.h2c.2 <- bayes_factor(b.h2c.2,b.h2c.0)
bf.h2c.21 <- bayes_factor(b.h2c.2,b.h2c.1)

log(as.numeric(bf.h2c.1[1]))
log(as.numeric(bf.h2c.2[1]))
log(as.numeric(bf.h2c.21[1]))


# predictor bayes factor check
# one direction -> positive
hypothesis(b.h2c.2,'consp<0')


# effect size
EMAtools::lme.dscore(h2c.1, type='lme4', data=data.filtered)

#### H2d/e
# null mediation?
model0_mediator <- bf(trust_6~ gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1|residing_country))
model0_outcome <- bf(vx ~  gender + education + work_location + age+
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
model1_mediator <- bf(trust_6~consp + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1|residing_country))
model1_outcome <- bf(vx ~ consp+trust_6 + gender + education + work_location + age+
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

# mediation model (full)
model_mediator <- bf(trust_6~consp + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1+consp|residing_country))
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


# compare models
bf.med.10 <- bayes_factor(med1_result, med0_result)
bf.med.20 <- bayes_factor(med_result, med0_result)
bf.med.21 <- bayes_factor(med_result, med1_result)

log(bf.med.21$bf[1])

save.image(file='Vaccine_H2.RData')
