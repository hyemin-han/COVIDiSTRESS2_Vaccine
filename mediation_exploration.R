# random slope exploration
library(lmerTest)

# load aligned data
load((file='Vaccine_aligned.RData'))

# standardize willingness and VAQB
data.filtered$vx <- scale(data.filtered$vx)
data.filtered$consp <- scale(data.filtered$consp)
data.filtered$anti <- scale(data.filtered$anti)
data.filtered$trust_6 <- scale(data.filtered$trust_6)
data.filtered$trust_7 <- scale(data.filtered$trust_7)

# H2 model
model.h2 <- lmer(vx ~ trust_6+consp+gender + education + work_location + age+
                            SSS_faml+ relationship_status+
                            (1+trust_6+consp|residing_country), data=data.filtered)

options(width = 2000)


library(sjPlot)
library(sjmisc)

# random slope plot
plot_model(model.h2,type='pred',terms=c('consp', 'residing_country'),pred.type='re',
           ci.lvl = NA,colors='BW')


# H3 model
model.h3 <- lmer(vx ~ trust_7+anti+gender + education + work_location + age+
                            SSS_faml+ relationship_status+
                            (1+trust_7+anti|residing_country), data=data.filtered)
