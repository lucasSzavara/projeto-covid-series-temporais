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

  vector a_b(int a, int b) {
      vector[b - a + 1] vec;
      int j = 1;
      for (i in a:b) {
          vec[j] = i;
          j += 1;
      }
      return vec;
  }

  real cdf_neg_binomial_family(int y, real mu, real sigma, real nu, real p0) {
      if (y < 0) return 0;
      real num = inc_beta(
          y + 1,
          pow(mu, 2 - nu) / sigma,
          sigma * pow(mu, nu - 1) / (1 + sigma * pow(mu, nu - 1))
      );
      real cdf_binom_neg = 1 - num;
      cdf_binom_neg = p0 + (1 - p0) * cdf_binom_neg;
      // real denom = beta(y + 1, pow(mu, 2 - nu) / sigma);
      return cdf_binom_neg;
      // vector[y + 1] y0 = a_b(0, y);
      // vector[y + 1] inv_beta_term = tgamma(y0 + pow(mu, 2 - nu) / sigma) ./ tgamma(y0 + 1);
      // vector[y + 1] term = pow(sigma * pow(mu, nu - 1), y0);
      // real constant = pow(1 / (1 + sigma * pow(mu, nu - 1)), pow(mu, 2 - nu) / sigma) / tgamma(pow(mu, 2 - nu) / sigma);
      // print("Log neg binom");
      // print(constant);
      // print(y);
      // print(mean(term));
      // print(max(term));
      // print(mean(inv_beta_term));
      // print(max(inv_beta_term));
      // print(sum(inv_beta_term .* term * constant));
      // return sum(inv_beta_term .* term * constant);
  }

  // real IG_lpdf(vector x, real mu, vector sigma) {
  //     vector[num_elements (x)] prob;
  //     real lprob;
  //     for (i in 1:num_elements(x)) {
  //         prob[i] = (1/(2*pi()*(sigma[i]^2)*(x[i]^3)))^0.5 * exp(-(x[i] - mu)^2/(2*mu^2*sigma[i]^2*x[i]));
  //     }
  //     lprob = sum(log(prob));
  //     return lprob;
  // }
  //
  // real IG_rng(real mu, real sigma) {
  //     real nu = normal_rng(0, 1);
  //     real y = nu ^ 2;
  //     real lambda = 1 / (sigma^2);
  //     real x = mu + ((mu^2 * y) / (2*lambda)) - ((mu / (2 * lambda)) * sqrt(4*mu*lambda*y + (mu * y)^2));
  //     real z = uniform_rng(0, 1);
  //     if (z <= (mu / (mu + x))) {
  //         return x;
  //     }
  //     return mu^2 / x;
  // }
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
  vector<lower=0, upper=populacao>[n_ondas] d;
  vector<lower=30, upper=N*1.2>[n_ondas] e_;
  vector<lower=1, upper=100>[n_ondas] b;
  vector<lower=0>[7] s;
  vector<lower=-1, upper=1>[p] phi;
  // vector<lower=-1, upper=1>[q] theta;
  // real<lower=0> sigma;
  real alpha;
  vector[7] beta;
  vector[7] alpha_sigma;
  // real<lower=0> sigma;
  real beta_sigma;
  vector<lower=0>[N] lambda;
  real<lower=0> nu;
  // real<lower=0> kappa;
  // vector<lower=0, upper=1>[N] p0;
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
  vector[N] beta_rep = rep(beta, N);
  // vector[N] beta_sigma_rep = rep(beta_sigma, N);
  vector[N] alpha_sigma_rep = rep(alpha_sigma, N);

  // matrix[N, n_ondas] np;
  // vector[N] np = rep_vector(0, N);
  // vector[N] pdf;
  vector[N] np = rep_vector(0, N);
  vector[N] pdf;
  for (k in 1:n_ondas) {
    //for (i in 1:N) {
    //  pdf[i] = s_normalized[i % 7 + 1] + loglogistic_lpdf(t[i] | e_cumsum[k], b[k]);
    //}
    pdf = (log(b[k]) - log(e_cumsum[k])) + log((t / e_cumsum[k]) ^ (b[k] - 1)) - log((1 + (t / e_cumsum[k]) ^ b[k]) ^ 2);   // loglogistic_lpdf(t[i] | e_cumsum[k], b[k]);
    np += exp(log(s_rep) + log(d[k]) + pdf);
  }

  vector[N] mu = log(np);

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
      eta_t[j] += phi[i] * (log(daily_deaths_[j - i]) - mu[j - i]);
      if (h == 1 && is_nan(eta_t[j])) {
        print(j, " foi transformado em NAN pelo AR com i = ", i);
        print(phi);
        print(mu[j - i]);
        print(log(daily_deaths_[j - i]));
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
  // var = mu + mu² / sigma
  // mu² / (var - mu) = sigma
  // vector[N] prob =  p_t * sigma;
  vector[N] sigma = exp(alpha_sigma_rep + mu .* beta_sigma);
  vector[N] p0 = inv_logit(alpha + mu .* beta_rep);
  vector[N] gamma_param = 1 / (sigma .* pow(exp(eta_t), (nu / 2 - 1)));
  // PIG: IG(1, sigma^(1/2))
  // vector[N] ig_param = sigma ^ (1/2);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real media_d = populacao / 1e5 * 340 / n_ondas;
  real variancia_d = 5000^2;

  real alpha_d = media_d ^ 2 / variancia_d;
  real beta_d = media_d / variancia_d;

  // print("d beta:", beta_d);
//  d ~ gamma(alpha_d, beta_d);

  s ~ gamma(10, 100);

  // real media_b = 20;
  // real variancia_b = (30)^2;
  //
  // real alpha_b = media_b ^ 2 / variancia_b;
  // real beta_b = media_b / variancia_b;
  //
  // b - 1 ~ gamma(alpha_b, beta_b);

  real media_e = (N - 100) / n_ondas;
  real variancia_e = 150^2;

  real alpha_e = media_e ^ 2 / variancia_e;
  real beta_e = media_e / variancia_e;
  e_ - 30 ~ gamma(alpha_e, beta_e);
  // phi ~ double_exponential(0, 1);
  // theta ~ normal(0, 2);

  // real media_s = 50;
  // real variancia_s = 10 * 50;
  //
  // real alpha_s = media_s ^ 2 / variancia_s;
  // real beta_s = media_s / variancia_s;
  // mu = 1
  // sigma_gamma = sqrt(sigma) * exp(eta) ^(nu/2 - 1)
  // alpha = 1 / sigma_gamma² = 1 / (sigma * exp(eta) ^(nu/2 - 1))
  // beta_j = mu * sigma_gamma²
  // beta_j = 1 * sigma
  // beta = 1 / beta_j = 1/sigma

  lambda ~ gamma(
      gamma_param,
      gamma_param
  );
  // lambda ~ IG(1, ig_param);
  alpha_sigma ~ normal(0, 1);
  nu ~ gamma(20, 20);
  // p0 ~ beta_proportion(mu_beta, kappa);
  // kappa ~ gamma(0.01, 0.01);

  for(n in (max(p, q) + 1):N) {
    if (daily_deaths[n] == 0) {
      target += log_sum_exp(log(p0[n]),
                            log1m(p0[n])
                              + poisson_lpmf(daily_deaths[n] | exp(eta_t[n]) * lambda[n]));
    } else {
      target += log1m(p0[n])
                  + poisson_lpmf(daily_deaths[n] | exp(eta_t[n]) * lambda[n]);
    }
  }
}

generated quantities {
  array[N] int y_gen;
  vector[N] residuo_quantil_normalizado;
  vector[N] b_res;

  for (i in 1:N) {
    real a_i;
    int e0 = bernoulli_rng(p0[i]);
    real lambdai = gamma_rng(gamma_param[i], gamma_param[i]);
    if (e0 == 1) {
        y_gen[i] = 0;
    } else {
        y_gen[i] = poisson_rng(exp(eta_t[i]) * lambdai);
    }
    a_i = cdf_neg_binomial_family(daily_deaths[i] - 1, exp(eta_t[i]), sigma[i], nu, p0[i]);
    b_res[i] = cdf_neg_binomial_family(daily_deaths[i], exp(eta_t[i]), sigma[i], nu, p0[i]);
    if (b_res[i] <= a_i) print("b <= a", a_i, b_res[i]);
    if (a_i < 0) {
        print("a < 0", a_i);
    }
    if (b_res[i] >= 1) {
        print("b >= 1", b_res[i]);
    }
    if (a_i == b_res[i]) {
      residuo_quantil_normalizado[i] = std_normal_qf(a_i);
      if (is_nan(residuo_quantil_normalizado[i])) print(a_i);
    } else {
      residuo_quantil_normalizado[i] = std_normal_qf(uniform_rng(a_i, b_res[i]));
      if (is_nan(residuo_quantil_normalizado[i])) print(a_i, b_res[i]);
    }
  }
}
