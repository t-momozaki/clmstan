# Prior Specification for clmstan

Create prior specifications for cumulative link models in clmstan.

**Default priors:**

- Regression coefficients (beta): `normal(0, 2.5)`

- Cutpoints (c): `normal(0, 10)` for flexible, `normal(0, 5)` for
  symmetric

- Interval (d): `gamma(2, 0.5)` for equidistant threshold

**Link parameter priors (when estimated):**

|              |                |                 |
|--------------|----------------|-----------------|
| Link         | Parameter      | Default Prior   |
| tlink        | df             | gamma(2, 0.1)   |
| aranda_ordaz | lambda         | gamma(0.5, 0.5) |
| gev          | xi             | normal(0, 2)    |
| sp           | r              | gamma(0.5, 0.5) |
| log_gamma    | lambda         | normal(0, 1)    |
| aep          | theta1, theta2 | gamma(2, 1)     |

## Usage

``` r
clm_prior(
  beta_sd = NULL,
  c_sd = NULL,
  c1_mu = NULL,
  c1_sd = NULL,
  d_alpha = NULL,
  d_beta = NULL,
  cpos_sd = NULL,
  df_alpha = NULL,
  df_beta = NULL,
  lambda_ao_alpha = NULL,
  lambda_ao_beta = NULL,
  lambda_lg_mu = NULL,
  lambda_lg_sd = NULL,
  xi_mu = NULL,
  xi_sd = NULL,
  r_alpha = NULL,
  r_beta = NULL,
  theta1_alpha = NULL,
  theta1_beta = NULL,
  theta2_alpha = NULL,
  theta2_beta = NULL
)
```

## Arguments

- beta_sd:

  SD for normal prior on regression coefficients. Default: 2.5 (weakly
  informative)

- c_sd:

  SD for normal prior on cutpoints (flexible threshold). Default: 10

- c1_mu:

  Mean for normal prior on first cutpoint (equidistant threshold).
  Default: 0

- c1_sd:

  SD for normal prior on first cutpoint (equidistant threshold).
  Default: 10

- d_alpha:

  Gamma shape for interval d (equidistant threshold). Default: 2

- d_beta:

  Gamma rate for interval d (equidistant threshold). Default: 0.5

- cpos_sd:

  SD for half-normal prior on positive cutpoints (symmetric threshold).
  Default: 5

- df_alpha:

  Gamma shape for tlink df. Default: 2

- df_beta:

  Gamma rate for tlink df. Default: 0.1

- lambda_ao_alpha:

  Gamma shape for aranda_ordaz lambda. Default: 0.5

- lambda_ao_beta:

  Gamma rate for aranda_ordaz lambda. Default: 0.5

- lambda_lg_mu:

  Normal mean for log_gamma lambda. Default: 0

- lambda_lg_sd:

  Normal SD for log_gamma lambda. Default: 1

- xi_mu:

  Normal mean for GEV xi. Default: 0

- xi_sd:

  Normal SD for GEV xi. Default: 2

- r_alpha:

  Gamma shape for SP r. Default: 0.5

- r_beta:

  Gamma rate for SP r. Default: 0.5

- theta1_alpha:

  Gamma shape for AEP theta1. Default: 2

- theta1_beta:

  Gamma rate for AEP theta1. Default: 1

- theta2_alpha:

  Gamma shape for AEP theta2. Default: 2

- theta2_beta:

  Gamma rate for AEP theta2. Default: 1

## Value

An object of class `"clm_prior"` containing prior specifications.

## Examples

``` r
# Create a prior object (does not require Stan)
my_prior <- clm_prior(beta_sd = 2, c_sd = 5)
print(my_prior)
#> clmstan prior specification:
#>   Regression coefficients (beta):
#>     beta_sd = 2
#>   Cutpoints (flexible):
#>     c_sd = 5

if (FALSE) { # \dontrun{
# Examples below require CmdStan and compiled Stan models
data(wine, package = "ordinal")

# Default priors (no customization needed)
fit <- clm_stan(rating ~ temp, data = wine,
                chains = 2, iter = 500, warmup = 250, refresh = 0)

# Custom prior for regression coefficients
fit2 <- clm_stan(rating ~ temp, data = wine,
                 prior = clm_prior(beta_sd = 1),
                 chains = 2, iter = 500, warmup = 250, refresh = 0)
} # }
```
