library(lmerTest)
library(brms)
library(EMAtools)

# load aligned data
load((file='Vaccine_aligned.RData'))

# H1: Vaccine willingness ~ VAQB?

# standardize willingness and VAQB
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$vaccine_0neutral <- scale(data.filtered$vaccine_0neutral)

# frequentist

# demographics to be determined....
# Age, gender, marital status, education level, SES, cohabitants, working from home

options(width=1000)
# model 0 (null model) demo and random intercepts
h1.0 <- lmer(vaccine_0neutral ~ gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 1 = model 1 + predictor
h1.1 <- lmer(vaccine_0neutral ~ vx+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1|residing_country), data=data.filtered)
# model 2 = model 1 + random slopes
h1.2 <- lmer(vaccine_0neutral ~ vx+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1+vx|residing_country), data=data.filtered)


# Bayesian
# prior -> normal distribution 
prior.coef <- brms::prior(cauchy(0.,1),class='b')

#model 0
b.h1.0 <- brms::brm(vaccine_0neutral ~ gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                           data=data.filtered, family = gaussian(),
                           cores=4,chains=4, save_pars = save_pars(all = T),
                           sample_prior ='yes', seed=1660415)

#model 1
b.h1.1 <- brms::brm(vaccine_0neutral ~ vx+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

#model 2
b.h1.2 <- brms::brm(vaccine_0neutral ~ vx+gender + education + work_location + age+
                      SSS_faml+ relationship_status+
                      (1+vx|residing_country),
                    data=data.filtered, family = gaussian(),
                    cores=4,chains=4, save_pars = save_pars(all = T),
                    sample_prior ='yes', seed=1660415,prior=prior.coef)

# model bayes factors
bf.h1.1 <- bayes_factor(b.h1.1,b.h1.0)
bf.h1.2 <- bayes_factor(b.h1.2,b.h1.0)
bf.h1.21 <- bayes_factor(b.h1.2,b.h1.1)

log(as.numeric(bf.h1.1[1]))
log(as.numeric(bf.h1.2[1]))
log(as.numeric(bf.h1.21[1]))

# model 2 best

# predictor bayes factor check
# one direction -> positive
hypothesis(b.h1.2,'vx>0')

# frequentist test as well
# vx: t = 26.4674881-01, df = 3.898e+01

pt(26.4674881, df = 3.898e+01,lower.tail = F)

# effect size
EMAtools::lme.dscore(h1.1, type='lme4', data=data.filtered)

save.image(file='Vaccine_H1.RData')
