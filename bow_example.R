# table 20.1
library(tidyverse)

# treatment A0 is randomly assigned at baseline
# A1 is assigned in month 1 with 
# pr(CD4 L1) = 0.4 if L1=0 (high), 0.8 if L1=1 (low)
# higher values of Y are better

N = c(2400, 1600, 2400, 9600, 4800, 3200, 1600, 6400)
A0 = c(0,0,0,0,1,1,1,1)
L1 = c(0,0,1,1,0,0,1,1)
A1 = c(0,1,0,1,0,1,0,1)
Ybar = c(84, 84, 52, 52, 76, 76, 44, 44)

df = data.frame(N=N, A0=A0, L1=L1, A1=A1, Ybar=Ybar)
summary(lm(Ybar ~ A0 + A1 + L1, data = df))

# in general, valid estimation of the effect of treatment strats
# possible when the joint effect can be est simultaneously

# in order to use modelling, need to hypothesize a dose-response fn
df$cumu = df$A0 + df$A1

m1 = lm(Ybar ~ cumu + L1,  data = df)

A0 = c(0,0,0,0,1,1,1,1)
Z1 = c(0,0,1,1,0,0,1,1)
A1 = c(0,1,0,1,0,1,0,1)
Y = c(87.29, 112.11, 119.65, 144.84, 105.28, 130.18, 137.72, 162.83)
N = c(209271, 93779, 60654, 136293, 134781, 60789, 93903, 210527)
df = data.frame(A0=A0, Z1=Z1, A1=A1, Y=Y, N=N)

m0 = lm(Y~A0 + Z1 + A1)
summary(m0)
m1 = glm(Z1 ~ A0, weights = N)

# treatment probabilities

# P(A1 = 1 | Z1 = 1)
pA11Z11 = 0.5 * sum(filter(df, A1==1 & Z1 == 1)$N)/sum( filter(df, Z1 == 1)$N )
pA11Z10 = 0.5 * sum(filter(df, A1==1 & Z1 == 0)$N)/sum( filter(df, Z1 == 0)$N )
pA10Z11 = 0.5 * sum(filter(df, A1==0 & Z1 == 1)$N)/sum( filter(df, Z1 == 1)$N )
pA10Z10 = 0.5 * sum(filter(df, A1==0 & Z1 == 0)$N)/sum( filter(df, Z1 == 0)$N )

pA11 = sum(filter(df, A1==1)$N)/sum(df$N)
pA10 = sum(filter(df, A1==0)$N)/sum(df$N)

tm = glm(A1 ~ Z1, weights = N, family = binomial) # treatment model
tm_sw = glm(A1 ~ 1, weights = N, family = binomial) # treatment model - stabilize weights

sw_num = ifelse(df$A1==1, plogis(coefficients(tm_sw))*.5, (1-plogis(coefficients(tm_sw))) * .5 )

sw_den = ifelse(df$A1 == 1, fitted.values(tm), 1- fitted.values(tm)) * .5

df$sw = sw/(.5 * fitted.values(tm))


pseudo_N = sw_num/sw_den * N

lm(Y ~ A0 + A1, weights = pseudo_N)





