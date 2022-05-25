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

# H3a trust_7 ~ anti

options(width=1000)
# model 0 (null model) demo and random intercepts
h3a.0 <- lmer(trust_7 ~ gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h3a.1 <- lmer(trust_7 ~ anti+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h3a.2 <- lmer(trust_7 ~ anti+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1+anti|residing_country), data=data.filtered)


# Bayesian
# prior -> normal distribution 
prior.coef <- brms::prior(cauchy(0.,1),class='b')

#model 0
b.h3a.0 <- brms::brm(trust_7 ~ gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                           data=data.filtered, family = gaussian(),
                           cores=4,chains=4, save_pars = save_pars(all = T),
                           sample_prior ='yes', seed=1660415)

#model 1
b.h3a.1 <- brms::brm(trust_7 ~ anti+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h3a.2 <- brms::brm(trust_7 ~ anti+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1+anti|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

b.h3a.2

# model bayes factors
bf.h3a.1 <- bayes_factor(b.h3a.1,b.h3a.0)
bf.h3a.2 <- bayes_factor(b.h3a.2,b.h3a.0)
bf.h3a.21 <- bayes_factor(b.h3a.2,b.h3a.1)

log(as.numeric(bf.h3a.1[1]))
log(as.numeric(bf.h3a.2[1]))
log(as.numeric(bf.h3a.21[1]))



# predictor bayes factor check
# one direction -> positive
hypothesis(b.h3a.2,'anti<0')


# effect size
EMAtools::lme.dscore(h3a.1, type='lme4', data=data.filtered)



#####
# H3b : vx ~ trust _ 7
# model 0 (null model) demo and random intercepts
h3b.0 <- lmer(vx ~ gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h3b.1 <- lmer(vx ~ trust_7+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h3b.2 <- lmer(vx ~ trust_7+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1+trust_7|residing_country), data=data.filtered)



#model 0
b.h3b.0 <- brms::brm(vx ~ gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415)

#model 1
b.h3b.1 <- brms::brm(vx ~ trust_7+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h3b.2 <- brms::brm(vx ~ trust_7+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1+trust_7|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)


b.h3b.2

# model bayes factors
bf.h3b.1 <- bayes_factor(b.h3b.1,b.h3b.0)
bf.h3b.2 <- bayes_factor(b.h3b.2,b.h3b.0)
bf.h3b.21 <- bayes_factor(b.h3b.2,b.h3b.1)

log(as.numeric(bf.h3b.1[1]))
log(as.numeric(bf.h3b.2[1]))
log(as.numeric(bf.h3b.21[1]))

hypothesis(b.h3b.2,'trust_7>0')

EMAtools::lme.dscore(h3b.1, type='lme4', data=data.filtered)

###
# H3c : vx ~ anti
# model 0 (null model) demo and random intercepts
h3c.0 <- lmer(vx ~ gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h3c.1 <- lmer(vx ~ anti+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h3c.2 <- lmer(vx ~ anti+gender + education + work_location + age+
                SSS_faml+ relationship_status+
                (1+anti|residing_country), data=data.filtered)


#model 0
b.h3c.0 <- brms::brm(vx ~ gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415)

#model 1
b.h3c.1 <- brms::brm(vx ~ anti+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h3c.2 <- brms::brm(vx ~ anti+gender + education + work_location + age+
                       SSS_faml+ relationship_status+
                       (1+anti|residing_country),
                     data=data.filtered, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef)

b.h3c.2

# model bayes factors
bf.h3c.1 <- bayes_factor(b.h3c.1,b.h3c.0)
bf.h3c.2 <- bayes_factor(b.h3c.2,b.h3c.0)
bf.h3c.21 <- bayes_factor(b.h3c.2,b.h3c.1)

log(as.numeric(bf.h3c.1[1]))
log(as.numeric(bf.h3c.2[1]))
log(as.numeric(bf.h3c.21[1]))


# predictor bayes factor check
# one direction -> positive
hypothesis(b.h3c.2,'anti<0')


# effect size
EMAtools::lme.dscore(h3c.1, type='lme4', data=data.filtered)

#### H3d/e
# null mediation?
model30_mediator <- bf(trust_7~ gender + education + work_location + age+
                        SSS_faml+ relationship_status + (1|residing_country))
model30_outcome <- bf(vx ~  gender + education + work_location + age+
                       SSS_faml+ relationship_status+ (1|residing_country))
options(width = 2000)
med30_result = brm(
  model30_mediator + model30_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)

# mediation without intercepts
model31_mediator <- bf(trust_7~anti + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1|residing_country))
model31_outcome <- bf(vx ~ anti+trust_7 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1|residing_country))
options(width = 2000)
med31_result = brm(
  model31_mediator + model31_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef
)
bayestestR::mediation(med31_result)

# mediation model (full)

model3_mediator <- bf(trust_7~anti + gender + education + work_location + age+
                       SSS_faml+ relationship_status + (1+anti|residing_country))
model3_outcome <- bf(vx ~ anti+trust_7 + gender + education + work_location + age+
                      SSS_faml+ relationship_status+ (1+trust_7+anti|residing_country))

med3_result = brm(
  model3_mediator + model3_outcome + set_rescor(F),
  data=data.filtered,
  family = gaussian(),
  cores=4,chains=4, save_pars = save_pars(all = T),
  sample_prior ='yes', seed=1660415,prior=prior.coef)

options(width = 2000)
summary(med3_result)
bayestestR::mediation(med3_result)


# compare models
bf.med3.10 <- bayes_factor(med31_result, med30_result)
bf.med3.20 <- bayes_factor(med3_result, med30_result)
bf.med3.21 <- bayes_factor(med3_result, med31_result)

log(bf.med3.21$bf[1])

save.image(file='Vaccine_H3.RData')
