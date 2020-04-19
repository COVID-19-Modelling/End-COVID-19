model {
  for (i in 1:n_t){
    dd[i] ~ dnorm(r_death * x1[i], 1/sqrt(tau_d))
    dr[i] ~ dnorm(r_rec * x1[i], 1/sqrt(tau_r))
  }

  tau_d ~ dgamma(1E-2, 1E-2)
  tau_r ~ dgamma(1E-2, 1E-2)

  cdg <- r_iso / (rate)
  rate <- r_iso + r_rec + r_death
  
  r_rec ~ dgamma(1E-2, 1E-2)
  r_death ~ dgamma(1E-2, 1E-2)
}