## Resubmission

This is a resubmission. In this version I have:

* Put software names in single quotes in DESCRIPTION Title and Description
  (e.g., 'CmdStanR', 'instantiate').
* Expanded all acronyms in the Description field
  (GEV = Generalized Extreme Value, AEP = Asymmetric Exponential Power).
* Added references in the Description field with DOI/ISBN
  (Agresti 2010, Wang and Dey 2011, Naranjo et al. 2015).
* Added \value tags to all exported print methods
  (print.clm_dist, print.clm_prior, print.clm_prior_spec, print.clm_prior_list).

## R CMD check results

0 errors | 0 warnings | 1 note

### NOTEs

* This is a new submission.

* Package suggests `cmdstanr` which is available from r-universe
  (https://stan-dev.r-universe.dev), as specified in `Additional_repositories`
  in DESCRIPTION.

### Regarding Compilation Flags WARNING (if reported)

The package may show a warning about non-portable compilation flags such as:

```
'-Wno-deprecated-declarations' '-Wno-ignored-attributes'
'-Wno-sign-compare' '-Wno-tautological-compare'
'-Wno-unknown-warning-option'
```

These flags are used internally by CmdStan/Stan when compiling Stan models
during package installation. They are not controlled by this package but are
part of the Stan ecosystem's build system. The `instantiate` package
(https://wlandau.github.io/instantiate/) handles the Stan model compilation,
and these flags are necessary for successful compilation of Stan's C++ code.

This is consistent with other R packages that interface with Stan.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* win-builder (R-devel)
* GitHub Actions (ubuntu-latest): R release
* GitHub Actions (macOS-latest): R release
* GitHub Actions (windows-latest): R release

## Package Purpose

`clmstan` provides Bayesian estimation for cumulative link models (ordinal
regression) using Stan via the `cmdstanr` interface. It offers:

- 11 link functions (5 standard + 6 flexible with shape parameters)
- 3 threshold structures (flexible, equidistant, symmetric)
- Optional Bayesian estimation of link function parameters
- Model comparison via LOO-CV and WAIC

## Dependencies

The package uses `instantiate` to pre-compile Stan models at installation time,
eliminating user wait time for model compilation. This requires:

- `cmdstanr` (Suggests, available from stan-dev.r-universe.dev)
- CmdStan (installed via `cmdstanr::install_cmdstan()`)
