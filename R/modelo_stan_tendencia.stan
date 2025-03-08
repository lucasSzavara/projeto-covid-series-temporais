//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


functions {

  vector rep(vector x, int length) {
    int N = num_elements(x);
    vector[length] y;
    int K = length %/% N;
    int mod = length % N;
    for (k in 1:K) {
      y[((k - 1)*N + 1):(k*N)] = x;
    }
    y[(K*N + 1):length] = x[1:mod];
    return y;
  }
}

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=1> n_ondas;
  int<lower=1> populacao;
  array[N] int daily_deaths;
  vector[N] t;
  int<lower=0> p;
  int<lower=0> q;
}

transformed data {
  vector[N] daily_deaths_; // = rep_vector(0, N);
  
  for (i in 1:N) {
    daily_deaths_[i] = max([daily_deaths[i], 0.5]);
  }
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector<lower=0>[n_ondas] d;
  vector<lower=30>[n_ondas] e_;
  vector<lower=1>[n_ondas] b;
  vector<lower=0>[7] s;
  vector<lower=-1, upper=1>[p] phi;
  vector<lower=-1, upper=1>[q] theta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[n_ondas] e_cumsum;
  e_cumsum = cumulative_sum(e_);
  vector[7] s_normalized = s / mean(s);
  for (i in 1:7) {
    if (is_nan(s_normalized[i])) {
      print(s);
      break;
    }
  }
  vector[N] s_rep = rep(s_normalized, N);
  
  matrix[N, n_ondas] np;
  vector[N] pdf;
  
  // print(d);
  for (k in 1:n_ondas) {
    if (is_nan(e_cumsum[k]) || is_inf(e_cumsum[k])) {
      print(e_);
    }
    // for (i in 1:N) {
    //  pdf[i] = log(s_rep[i]) + loglogistic_lpdf(t[i] | e_cumsum[k], b[k]);
    // }
    pdf = log(s_rep) + (log(b[k]) - log(e_cumsum[k])) + log((t / e_cumsum[k]) ^ (b[k] - 1)) - log((1 + (t / e_cumsum[k]) ^ b[k]) ^ 2);   // loglogistic_lpdf(t[i] | e_cumsum[k], b[k]);
    for (i in 1:N) {
      if (is_nan(pdf[i]) || is_inf(pdf[i])) {
        print("pdf Onda", k, "T=", i);
        print(log(s_normalized));
        print((i / e_cumsum[k]) ^ (b[k] - 1));
        print((1 + (i / e_cumsum[k]) ^ b[k]) ^ 2);
        print(s);
        print(b);
        print(e_cumsum);
        print(pdf[i]);
        break;
      }
    }
    np[, k] = log(d[k]) + pdf;
  }
  
  vector[N] mu;
  for (i in 1:N) {
    mu[i] = log_sum_exp(np[i]);
  }
  
  
  // AR: 
  vector[N] eta_t = mu;
  eta_t[1:max(p, q)] = log(daily_deaths_[1:max(p, q)]);
  
  for (i in 1:N) {
    if (is_nan(eta_t[i])) {
      print("eta i=", i);
      break;
    }
  }
  
  for (j in (max(p, q) + 1):N) {
    real h = 0;
    if (!is_nan(eta_t[j])) {
      h = 1;
    }
    for (i in 1:p) {
      eta_t[j] += phi[i] * (log(daily_deaths_[j]) - mu[j]);
      if (h == 1 && is_nan(eta_t[j])) {
        print(j, " foi transformado em NAN pelo AR com i = ", i);
        print(phi);
        print(mu[j]);
        print(log(daily_deaths_[j]));
        break;
      }
    }
    if (h == 1 && is_nan(eta_t[j])) {
      print(j, " foi transformado em NAN pelo AR");
      break;
    }
  }
  for (i in 1:N) {
    if (is_nan(eta_t[i])) {
      print("eta i=", i, "pos ar");
      break;
    }
  }
  
  // MA:
  // vector[N] epsilon;
  // 
  // for (i in (max(p, q) + 1):N) {
  //   epsilon[i] = log(daily_deaths_[i]) - eta_t[i];
  //   for (j in 1:min(i - 1, q)) {
  //     epsilon[i] -= theta[j] * epsilon[i - j];
  //   }
  //   // eta_t[i:(i+min(N - i, q))] += theta[1:(min(N - i, q) + 1)] * epsilon[i];
  // }
  // 
  // for (i in 1:N) {
  //   for (j in 1:min(i - 1, q)) {
  //     eta_t[i] += theta[j] * epsilon[i - j];
  //   }
  // }
  // for (i in 1:N) {
  //   if (is_nan(eta_t[i])) {
  //     print("eta i=", i, " pos ma");
  //     break;
  //   }
  // }
  // vector[N] p_t = exp(eta_t);
  // vector[N] prob =  p_t * sigma;
  // vector[N] beta = sigma * (1 - p_t);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real media_d = 50000 / n_ondas;
  real variancia_d = 5000^2;

  real alpha_d = media_d ^ 2 / variancia_d;
  real beta_d = media_d / variancia_d;

  // print("d beta:", beta_d);
  d ~ gamma(alpha_d, beta_d);
  
  s ~ gamma(10, 100);
  
  real media_b = 20 * n_ondas;
  real variancia_b = (30 * n_ondas)^2;
 
  real alpha_b = media_b ^ 2 / variancia_b;
  real beta_b = media_b / variancia_b;
  
  b - 1 ~ gamma(alpha_b, beta_b);

  // real media_e = (N - 100) %/% n_ondas;
  // real variancia_e = 900;
  // 
  // real alpha_e = media_e ^ 2 / variancia_e;
  // real beta_e = media_e / variancia_e;
  // e_ - 30 ~ gamma(alpha_e, beta_e);
  // phi ~ normal(0, 2);
  // theta ~ normal(0, 2);
  
  real media_s = 50;
  real variancia_s = 10 * 50;
  
  real alpha_s = media_s ^ 2 / variancia_s;
  real beta_s = media_s / variancia_s;
  sigma ~ gamma(alpha_s, beta_s);
  
  
  daily_deaths[(max(p, q) + 1):N] ~ neg_binomial_2_log(eta_t[(max(p, q) + 1):N], sigma);
}

// generated quantities {
//  vector[N] y_test;
//  for(i in 1:N) {
//    y_test[i] = neg_binomial_2_logrng(inv_logit(alpha + beta*x_test[i]));
//  }
// }
