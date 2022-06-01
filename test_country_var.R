# country level analysis
# random slopes

library(lmerTest)

# load aligned data
load((file='Vaccine_aligned.RData'))

# standardize willingness and VAQB
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$consp <- scale(data.filtered$consp)
data.filtered$anti <- scale(data.filtered$anti)
data.filtered$trust_6 <- scale(data.filtered$trust_6)
data.filtered$trust_7 <- scale(data.filtered$trust_7)
data.filtered$vaccine_0neutral <- scale(data.filtered$vaccine_0neutral)

h1.2 <- lmer(vaccine_0neutral ~ vx+gender + education + work_location + age+
               SSS_faml+ relationship_status+
               (1+vx|residing_country), data=data.filtered)

# H1

# get H1 (vaccine ~ vx) slopes
H1.coefs <- coef(h1.2)$residing_country$vx

# report result
H1.summary <- cbind(as.data.frame(labels(table(data.filtered$residing_country))[[1]]),
                    as.data.frame(H1.coefs))
colnames(H1.summary) <- c('Country','vx')
# vaccine ~ vx
H1.summary <- H1.summary[order(-H1.summary$vx),]




# get H2 coefs
h2c.model <- lmer(vx ~ consp+trust_6 + gender + education + work_location + age+
                    SSS_faml+ relationship_status+ (1+trust_6+consp|residing_country),
                  data=data.filtered)
H2.coefs.consp <- coef(h2c.model)$residing_country$consp
H2.coefs.trust_6 <- coef(h2c.model)$residing_country$trust_6

H2.consp.summary <- cbind(as.data.frame(labels(table(data.filtered$residing_country))[[1]]),
                          as.data.frame(H2.coefs.consp))
colnames(H2.consp.summary) <- c('Country','consp')
# vx ~ consp
H2.consp.summary <- H2.consp.summary[order(-H2.consp.summary$consp),]

H2.trust_6.summary <- cbind(as.data.frame(labels(table(data.filtered$residing_country))[[1]]),
                          as.data.frame(H2.coefs.trust_6))
colnames(H2.trust_6.summary) <- c('Country','trust_6')
# vx ~ consp
H2.trust_6.summary <- H2.trust_6.summary[order(-H2.trust_6.summary$trust_6),]


# H3


# get H3 coefs
h3c.model <- lmer(vx ~ anti+trust_7 + gender + education + work_location + age+
                    SSS_faml+ relationship_status+ (1+trust_7+anti|residing_country),
                  data=data.filtered)
H3.coefs.anti<- coef(h3c.model)$residing_country$anti
H3.coefs.trust_7 <- coef(h3c.model)$residing_country$trust_7

H3.anti.summary <- cbind(as.data.frame(labels(table(data.filtered$residing_country))[[1]]),
                          as.data.frame(H3.coefs.anti))
colnames(H3.anti.summary) <- c('Country','anti')
# vx ~ consp
H3.anti.summary <- H3.anti.summary[order(-H3.anti.summary$anti),]

H3.trust_7.summary <- cbind(as.data.frame(labels(table(data.filtered$residing_country))[[1]]),
                            as.data.frame(H3.coefs.trust_7))
colnames(H3.trust_7.summary) <- c('Country','trust_7')
# vx ~ consp
H3.trust_7.summary <- H3.trust_7.summary[order(-H3.trust_7.summary$trust_7),]


# Bayesian trial
library(brms)
library(bayestestR)

# H1
prior.coef <- brms::prior(cauchy(0.,1),class='b')
b.h1 <- brms::brm(vaccine_0neutral ~ vx+gender + education + work_location + age+
                    SSS_faml+ relationship_status+
                    (1+vx|residing_country),
                  data=data.filtered, family = gaussian(),
                  cores=4,chains=4, save_pars = save_pars(all = T),
                  sample_prior ='yes', seed=1660415,prior=prior.coef)
btable.h1 <- describe_posterior(b.h1, effect='random')

# save result
write.csv(as.data.frame(btable.h1),'H1.csv')

# H2
prior.coef <- brms::prior(cauchy(0.,1),class='b')
b.h2 <- brms::brm(vx ~ consp+trust_6+gender + education + work_location + age+
                    SSS_faml+ relationship_status+
                    (1+consp+trust_6|residing_country),
                  data=data.filtered, family = gaussian(),
                  cores=4,chains=4, save_pars = save_pars(all = T),
                  sample_prior ='yes', seed=1660415,prior=prior.coef)

# get tables
btable.h2 <- describe_posterior(b.h2, effect='random')

# save result
write.csv(as.data.frame(btable.h2),'H2.csv')

# H3
prior.coef <- brms::prior(cauchy(0.,1),class='b')
b.h3 <- brms::brm(vx ~ anti+trust_7+gender + education + work_location + age+
                    SSS_faml+ relationship_status+
                    (1+anti+trust_7|residing_country),
                  data=data.filtered, family = gaussian(),
                  cores=4,chains=4, save_pars = save_pars(all = T),
                  sample_prior ='yes', seed=1660415,prior=prior.coef)

# get tables
btable.h3 <- describe_posterior(b.h3, effect='random')
# save result
write.csv(as.data.frame(btable.h3),'H3.csv')

save.image('coefs.RData')
