library(psych)
library(lavaan)
library(sirt)
library(MASS)

# function for factor score adjustment
aligned.factor.scores <- function(lambda,nu,y){
  # calculate inverse matrix
  lambda1 <- ginv((lambda))
  # create matrix for nu
  ns <- nrow(y)
  nus <- matrix(nu,nrow=ns,ncol=length(nu),byrow=T)
  # y - nu
  y_nu <- y - nus
  # f = inv(lambda)*(y-nu)
  F <- lambda1 %*% t(as.matrix(y_nu))
}

# extract Vax_related variable names
# without 4
vaxs <- c('vaccine_attitudes_9_midneutral','vaccine_attitudes_2_midneutral',
          'vaccine_attitudes_3_midneutral',
          'vaccine_attitudes_5_midneutral','vaccine_attitudes_6_midneutral')



# Load the cleaned csv file

# load data
data<-read.csv('Final_COVIDiSTRESS_Vol2_cleaned.csv')

# reverse coding
data$vaccine_attitudes_4_midneutral <- 8-data$vaccine_attitudes_4_midneutral
data$vaccine_attitudes_5_midneutral <- 8-data$vaccine_attitudes_5_midneutral

# extract languages with n >= 100
n.langs <- table(data$UserLanguage)
list.langs <- labels(n.langs)[[1]]
langs.include <- list.langs[n.langs>=100]
n.include <- n.langs[n.langs>=100]

# extract data
for (i in 1:length(langs.include)){
  if (i == 1){
    data.mi <- data[data$UserLanguage == langs.include[i],]
  }else{
    current <- data[data$UserLanguage == langs.include[i],]
    data.mi <- rbind(data.mi,current)
  }
}

# set and examine fitmeasures
fits <- c('rmsea.scaled','srmr','cfi.scaled','tli.scaled')

#####
# 1. VX

# general CFA: VX
cfa.model.vx <- 'VX =~ vaccine_attitudes_9_midneutral + 
vaccine_attitudes_2_midneutral+ vaccine_attitudes_3_midneutral+
vaccine_attitudes_5_midneutral+vaccine_attitudes_6_midneutral'
cfa.whole.vx <- cfa(model=cfa.model.vx,data=data.mi,estimator='WLSMV', group = 
                       'UserLanguage')
fitMeasures(cfa.whole.vx)[fits]
# msea.scaled         srmr   cfi.scaled   tli.scaled 
#    0.08972321   0.02870422   0.93010890   0.86021779 
# not good -> alignment

# measurement alignment test
# extract parameters
par.vx <- invariance_alignment_cfa_config(dat = data.mi[,vaxs], 
                                       group = data.mi$UserLanguage)
# do alignment
mod1.vx <- invariance.alignment(lambda = par.vx$lambda, nu =
                                   par.vx$nu, align.scale = c(0.2, 0.4), align.pow = c(0.25, 0.25))
# test performance
mod1.vx$es.invariance['R2',]
#loadings intercepts 
# 0.9675333  0.9968223    good

#####
# 2. Conspiratorial
item.consp <- c('conspirational_think_1','conspirational_think_2',
               'conspirational_think_3','conspirational_think_4')
cfa.consp<-'
CONSP =~ conspirational_think_1 + conspirational_think_2+
  conspirational_think_3 + conspirational_think_4'

# configural invariance
# DWLS preferred with 5+ categories (Christine DiStefano & Grant B. Morgan 2014)
fit.consp <- cfa(cfa.consp, data.mi, estimator = 'WLSMV', group='UserLanguage')
# fit indices
fitMeasures(fit.consp)[fits]

# no good rmsea -> alignment
#rmsea.scaled         srmr   cfi.scaled   tli.scaled 
#0.15744704   0.03697213   0.94147075   0.82441225 

# measurement alignment test
# extract parameters
par.consp <- invariance_alignment_cfa_config(dat = data.mi[,item.consp], 
                                          group = data.mi$UserLanguage)
# do alignment
mod1.consp <- invariance.alignment(lambda = par.consp$lambda, nu =
                                  par.consp$nu, align.scale = c(0.2, 0.4), align.pow = c(0.25, 0.25))
# test performance
mod1.consp$es.invariance['R2',]
#loadings intercepts 
# 0.9705771  0.9860852    good

#####
# 3. Antiexpert
items.anti <- c('antiexpert_1','antiexpert_2','antiexpert_3')
cfa.model.anti <- 'ANTI =~ antiexpert_1 + antiexpert_2+antiexpert_3'
cfa.whole.anti <- cfa(model=cfa.model.anti,data=data.mi,estimator='WLSMV', group = 
                      'UserLanguage')
fitMeasures(cfa.whole.anti)[fits]
# msea.scaled         srmr   cfi.scaled   tli.scaled 
#    0.08972321   0.02870422   0.93010890   0.86021779 

# good. metric?
cfa.metric.anti <- cfa(model=cfa.model.anti,data=data.mi,estimator='WLSMV', group = 
                         'UserLanguage', group.equal='loadings')
fitMeasures(cfa.metric.anti)[fits]
# not good. MA
#rmsea.scaled         srmr   cfi.scaled   tli.scaled 
#0.09410097   0.04009779   0.93973035   0.90624721

# measurement alignment test
# extract parameters
par.anti <- invariance_alignment_cfa_config(dat = data.mi[,items.anti], 
                                          group = data.mi$UserLanguage)
# do alignment
mod1.anti <- invariance.alignment(lambda = par.anti$lambda, nu =
                                  par.anti$nu, align.scale = c(0.2, 0.4), align.pow = c(0.25, 0.25))
# test performance
mod1.anti$es.invariance['R2',]
#loadings intercepts 
# 0.8592692  0.9875114     good

#####
# factor score calculation

for (i in 1:length(langs.include)){
  if (i == 1){
    # create new matrix
    data.aligned <- data.mi[data.mi$UserLanguage==langs.include[i],]
    # aligned factor score
    F.vx <- aligned.factor.scores(mod1.vx$lambda.aligned[i,],
                               mod1.vx$nu.aligned[i,],
                               data.mi[data.mi$UserLanguage==langs.include[i],vaxs])
    F.consp <- aligned.factor.scores(mod1.consp$lambda.aligned[i,],
                                   mod1.consp$nu.aligned[i,],
                                   data.mi[data.mi$UserLanguage==langs.include[i],item.consp])
    F.anti <- aligned.factor.scores(mod1.anti$lambda.aligned[i,],
                                 mod1.anti$nu.aligned[i,],
                                   data.mi[data.mi$UserLanguage==langs.include[i],items.anti])
#    F.rs <- aligned.factor.scores(mod1.resilience$lambda.aligned[i,],
#                                  mod1.resilience$nu.aligned[i,],
#                                  data.mi[data.mi$UserLanguage==langs.include[i],items.resilience])
#    F.ps <- aligned.factor.scores(mod1.ps$lambda.aligned[i,],
#                                  mod1.ps$nu.aligned[i,],
#                                  data.mi[data.mi$UserLanguage==langs.include[i],items.ps])
    data.aligned$vx <- t(F.vx)
    data.aligned$consp <- t(F.consp)
    data.aligned$anti <- t(F.anti)
#    data.aligned$resilience <- t(F.id)
#    data.aligned$primary_stressor_avg <- t(F.ps)
  }else
  {
    # bind
    current <- data.mi[data.mi$UserLanguage==langs.include[i],]
    F.vx <- aligned.factor.scores(mod1.vx$lambda.aligned[i,],
                                   mod1.vx$nu.aligned[i,],
                               current[,vaxs])
    F.consp <- aligned.factor.scores(mod1.consp$lambda.aligned[i,],
                                   mod1.consp$nu.aligned[i,],
                                   current[,item.consp])
    F.anti <- aligned.factor.scores(mod1.anti$lambda.aligned[i,],
                                  mod1.anti$nu.aligned[i,],
                                   current[,items.anti])
##    F.rs <- aligned.factor.scores(mod1.resilience$lambda.aligned[i,],
#                                  mod1.resilience$nu.aligned[i,],
#                                  current[,items.resilience])
#    F.ps <- aligned.factor.scores(mod1.ps$lambda.aligned[i,],
#                                  mod1.ps$nu.aligned[i,],
#                                  current[,items.ps])
    current$vx <- t(F.vx)
    current$consp <- t(F.consp)
    current$anti <- t(F.anti)
#    current$resilience <- t(F.rs)
#    current$primary_stressor_avg <- t(F.ps)
    data.aligned <- rbind(data.aligned,current)
  }
}


# filtering by country n >= 30
country.30 <- table(data.aligned$residing_country) >= 30

n.country <- table(data.aligned$residing_country)
list.country <- labels(n.country)[[1]]
country.include <- list.country[n.country>=30]
n.include.c <- n.country[n.country>=30]

# extract data
for (i in 1:length(country.include)){
  if (i == 1){
    data.filtered <- data.aligned[(data.aligned$residing_country == 
                                    country.include[i]) & !is.na(
                                      data.aligned$residing_country
                                    ),]
  }else{
    current <- data.aligned[(data.aligned$residing_country == country.include[i])
                            & !is.na(
                              data.aligned$residing_country
                            ),]
    data.filtered <- rbind(data.filtered,current)
  }
}


# save aligned datafile
save.image(file='Vaccine_aligned.RData')


# alphas
psych::alpha(data[,vaxs],check.keys=TRUE)
psych::alpha(data[,item.consp],check.keys=TRUE)
psych::alpha(data[,items.anti],check.keys=TRUE)

psych::alpha(data[,items.anti],check.keys=TRUE)
psych::alpha(data[,items.ps],check.keys=TRUE)