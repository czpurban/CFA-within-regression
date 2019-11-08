data{
  // n_subj: number of subjects
  int n_subj ;
  // n_y: number of outcomes
  int n_y ;
  // y: matrix of outcomes
  matrix[n_subj,n_y] y ;
  // n_fac: number of latent factors
  int n_fac ;
  // y_fac: list of which factor is associated with each outcome
  int<lower=1,upper=n_fac> y_fac[n_y] ;
  //observed DV in the regression
  real z [n_subj];
}
transformed data{
  // scaled_y: z-scaled outcomes
  matrix[n_subj,n_y] scaled_y ;
  for(i in 1:n_y){
    scaled_y[,i] = (y[,i]-mean(y[,i])) / sd(y[,i]) ;
  }
}
parameters{
  // normal01: a helper variable for more efficient non-centered multivariate
  //  sampling of subj_facs
  matrix[n_fac, n_subj] normal01;
  // fac_cor_helper: correlations (on cholesky factor scale) amongst latent
  //  factors
  cholesky_factor_corr[n_fac] fac_cor_helper ;
  // scaled_y_means: means for each outcome
  vector[n_y] scaled_y_means ;
  // scaled_y_noise: measurement noise for each outcome
  vector<lower=0>[n_y] scaled_y_noise ;
  // betas: how each factor loads on to each outcome.
  //  Must be postive for identifiability.
  vector<lower=0>[n_y] betas ;
  real gamma;
  real <lower=0> sigma;		
}
transformed parameters{
  // subj_facs: matrix of values for each factor associated with each subject
  matrix[n_subj,n_fac] subj_facs ;
  // the following calculation implies that rows of subj_facs are sampled from
  //  a multivariate normal distribution with mean of 0 and sd of 1 in all
  //  dimensions and a correlation matrix of fac_cor.
  subj_facs = transpose(fac_cor_helper * normal01) ;
}
model{
  // normal01 must have normal(0,1) prior for "non-centered" parameterization
  //  on the multivariate distribution of latent factors
  to_vector(normal01) ~ normal(0,1) ;
  // flat prior on correlations
  fac_cor_helper ~ lkj_corr_cholesky(1) ;
  // normal prior on y means
  scaled_y_means ~ normal(0,1) ;
  // weibull prior on measurement noise
  scaled_y_noise ~ weibull(2,1) ;
  // normal(0,1) priors on betas
  betas ~ normal(0,1) ;
  // each outcome as normal, influenced by its associated latent factor
  for(i in 1:n_y){
    scaled_y[,i] ~ normal(
      scaled_y_means[i] + subj_facs[,y_fac[i]] * betas[i]
      , scaled_y_noise[i]
    ) ;
  }
  for(j in 1:n_subj){
    z[j] ~ normal (gamma*subj_facs[j,1], sigma);  
  }

}
generated quantities{
  //cors: correlations amongst within-subject predictors' effects
  corr_matrix[n_fac] fac_cor ;
  // y_means: outcome means on the original scale of Y
  vector[n_y] y_means ;
  // y_noise: outcome noise on the original scale of Y
  vector[n_y] y_noise ;
  fac_cor = multiply_lower_tri_self_transpose(fac_cor_helper) ;
  for(i in 1:n_y){
    y_means[i] = scaled_y_means[i]*sd(y[,i]) + mean(y[,i]) ;
    y_noise[i] = scaled_y_noise[i]*sd(y[,i]) ;
  }
}