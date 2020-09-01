# table 20.1

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

