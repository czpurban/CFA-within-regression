#adopted from https://gist.github.com/mike-lawrence/dd2435f290a567bd1fd03370ee669688
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rstan, TAM, car, lavaan, readxl, stringr, lme4, reshape2, boot, tidyverse)
rstan_options(auto_write = TRUE)

N<-1000 #sample size

#simulating latent factor
y<-rnorm(N, mean=0, sd = 3)

#simulating outcome variables (i.e., responses on the attitude scales)
#as a function of the unobserved latent factor
y1<-round(rnorm(N, mean = y-1, sd=2), digits=0)
y2<-round(rnorm(N, mean = y, sd=2), digits=0)
y3<-round(rnorm(N, mean = y+1, sd=2), digits=0)

#here I just make the outcome variables discrete (i.e., response scale -3...+3)
y1[y1<(-3)]<-(-3)
y1[y1>3]<-3
y2[y2<(-3)]<-(-3)
y2[y2>3]<-3
y3[y3<(-3)]<-(-3)
y3[y3>3]<-3

#simulating observed dependent variable for the regression model 
z<-rnorm(N, mean = y, sd=8)

# Regression of the observed DV on the unobserved latent variable ---------
lm(z~scale(y)) #this is OLS estimate of the regression weight for the effect
# of the latent variable on the observed DV; practically the same as the one obtained with the 
# bayesian model

# Bayesian model -----------------------------------------------------------

rstan_options(auto_write = TRUE)

data_for_stan = list(
  # n_y: number of outcomes
  n_y = 3
  # n_subj: number of subjects
  , n_subj = 1000
  # y: matrix of outcomes
  , y = as.matrix(cbind(y1, y2, y3))
  # n_fac: number of latent factors
  , n_fac = 1
  # y_fac: list of which factor is associated with each outcome
  , y_fac = c(1,1,1)
  , z = z
)

#setting the path to the working directory where the RStan file is stored
wd<-"C:\\Users\\urban.j\\Disk Google\\CUK\\analyzy\\analyzy R\\CFA within regression model" 
setwd(wd)

# Sample the model
post = rstan::stan(
  file = 'cfa_regr.stan'
  , data = data_for_stan
  , seed = 1
  , cores = 4
  , chains = 4
  , iter = 2e3
  , pars = c('normal01','fac_cor_helper')
  , include = FALSE
)

# betas: magnitude of relationship between each observed outcome on the attitude scale & it's associated
#  latent factor
print(
  post
  , pars = 'betas'
  , probs = c(.025,.975)
  , digits = 2
  , use_cache = FALSE
)

#this is the Bayesian estimate of the regression weigt for the effect of the latent factor
# on the observed dependent variable; the model actually recovers the effect from line 29 nicely
print(
  post
  , pars = 'gamma'
  , probs = c(.025,.975)
  , digits = 2
  , use_cache = FALSE
)

#this might be useful if we have more complicated factorial structure
# # fac_cor: correlations amongst latent factors
# print(
#   post
#   , pars = 'fac_cor'
#   , probs = c(.025,.975)
#   , digits = 2
#   , use_cache = FALSE
# )
# 
# # y_means: outcome means
# print(
#   post
#   , pars = 'y_means'
#   , probs = c(.025,.975)
#   , digits = 2
#   , use_cache = FALSE
# )
# 
# # y_means: outcome measurement noise
# print(
#   post
#   , pars = 'y_noise'
#   , probs = c(.025,.975)
#   , digits = 2
#   , use_cache = FALSE
# )



