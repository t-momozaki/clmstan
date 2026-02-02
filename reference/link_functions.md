# Available Link Functions

clmstan supports the following link functions for cumulative link
models:

**Standard links (no additional parameters):**

- `"logit"` - Logistic (proportional odds model)

- `"probit"` - Normal (latent variable interpretation)

- `"cloglog"` - Complementary log-log (proportional hazards)

- `"loglog"` - Log-log (Gumbel minimum)

- `"cauchit"` - Cauchy (heavy tails)

**Flexible links (with additional parameters):**

- `"tlink"` - Student-t (df \> 0)

  - df = Inf: equals probit

  - df \< 3: increasingly heavy tails; df \> 30 is nearly normal

- `"aranda_ordaz"` - Aranda-Ordaz asymmetric (lambda \> 0)

  - lambda = 1: equals logit

  - lambda -\> 0: approaches cloglog

- `"gev"` - Generalized extreme value (shape parameter xi)

  - xi = 0: Gumbel (equals loglog)

  - xi \< 0: Weibull (short tail)

  - xi \> 0: Frechet (heavy tail)

- `"sp"` - Symmetric power (r \> 0, base distribution)

  - r = 1: equals base distribution

  - 0 \< r \< 1: positively skewed

  - r \> 1: negatively skewed

- `"log_gamma"` - Log-gamma (lambda)

  - lambda = 0: equals probit

  - lambda \> 0 or \< 0: asymmetric

- `"aep"` - Asymmetric exponential power (theta1 \> 0, theta2 \> 0)

  - alpha = 0.5 fixed for identifiability

  - theta1 = theta2: symmetric distribution

  - theta = 2: Gaussian kernel (but NOT equal to probit due to scaling)

  - theta \< 2: heavy tails (leptokurtic)

  - theta \> 2: light tails (platykurtic)

## Link Parameter Specification

Flexible link parameters can be either **fixed** or **estimated**
(inferred).

**Fixed parameters:** Specify a numeric value

    clm_stan(y ~ x, link = "tlink", link_param = list(df = 8))
    clm_stan(y ~ x, link = "gev", link_param = list(xi = 0))  # equals loglog
    clm_stan(y ~ x, link = "aep", link_param = list(theta1 = 2, theta2 = 2))  # symmetric

**Estimated parameters:** Use `"estimate"` (with default prior)

    clm_stan(y ~ x, link = "tlink", link_param = list(df = "estimate"))
    clm_stan(y ~ x, link = "gev", link_param = list(xi = "estimate"))

**Custom priors:** Combine `"estimate"` with `prior` argument

    clm_stan(y ~ x, link = "gev",
             link_param = list(xi = "estimate"),
             prior = prior(normal(0, 0.3), class = "xi"))

## Default Priors for Link Parameters

When using `"estimate"`, the following default priors are used:

|              |           |                 |                                       |
|--------------|-----------|-----------------|---------------------------------------|
| Link         | Parameter | Default Prior   | Notes                                 |
| tlink        | df        | gamma(2, 0.1)   | Mode around 10, allows heavy tails    |
| aranda_ordaz | lambda    | gamma(0.5, 0.5) | Centered near 1 (logit)               |
| gev          | xi        | normal(0, 2)    | Weakly informative, Wang & Dey (2011) |
| sp           | r         | gamma(0.5, 0.5) | Centered near 1 (base distribution)   |
| log_gamma    | lambda    | normal(0, 1)    | Centered at 0 (probit)                |
| aep          | theta1    | gamma(2, 1)     | Mode at 1, symmetric at theta1=theta2 |
| aep          | theta2    | gamma(2, 1)     | Mode at 1, symmetric at theta1=theta2 |

## SP Link Details (Li et al., 2019)

The Symmetric Power link uses a symmetric base distribution F_0,
specified via the `base` argument. Supported bases:

- `base = "logit"`: Logistic base (default)

- `base = "probit"`: Normal base

- `base = "cauchit"`: Cauchy base

- `base = "tlink"`: Student-t base (requires df)

Note: Li et al. (2019) define F_0 as a CDF "whose corresponding PDF is
symmetric about 0".
