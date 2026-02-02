# MCMC Diagnostics for clmstan objects

Provides a summary of MCMC convergence diagnostics including
HMC-specific diagnostics (divergences, treedepth, E-BFMI) and general
convergence measures (Rhat, ESS).

## Usage

``` r
diagnostics(object, ...)

# S3 method for class 'clmstan'
diagnostics(
  object,
  detail = FALSE,
  rhat_threshold = 1.01,
  ess_threshold = 400,
  ...
)
```

## Arguments

- object:

  A clmstan object

- ...:

  Additional arguments (ignored)

- detail:

  Logical. If TRUE, show full parameter-level diagnostics table. If
  FALSE (default), show only summary and any problematic parameters.

- rhat_threshold:

  Threshold for flagging high Rhat values. Default 1.01.

- ess_threshold:

  Threshold for flagging low ESS values. Default 400.

## Value

Invisibly returns a list containing:

- hmc: HMC diagnostics from CmdStanMCMC\$diagnostic_summary()

- convergence: Data frame of per-parameter Rhat and ESS values

- issues: Logical indicating whether any issues were detected

## Details

The function checks for the following issues:

- **Divergences**: Number of divergent transitions (ideally 0)

- **Treedepth**: Transitions hitting max treedepth (efficiency issue)

- **E-BFMI**: Energy Bayesian Fraction of Missing Information (values \<
  0.3 indicate problems)

- **Rhat**: Potential scale reduction factor (values \> 1.01 indicate
  lack of convergence)

- **ESS**: Effective sample size for bulk and tail (low values indicate
  high autocorrelation)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- clm_stan(rating ~ temp, data = wine)
diagnostics(fit)
diagnostics(fit, detail = TRUE)
} # }
```
