## Resubmission

This is a patch release (0.1.2) that addresses the CRAN check failures
reported for the package (deadline 2026-07-20).

The failures are caused by an upcoming change in the `loo` package: the
output of `loo_compare()` is changing from a matrix to a data frame (with
additional diagnostic columns). Two integration tests in this package
asserted `is.matrix()` on that output and therefore began failing once the
new `loo` became available on some check platforms.

Changes in this version:

* Updated the affected tests to accept both the previous (matrix) and the
  new (data frame) output of `loo_compare()`, so the package works with both
  the current and the upcoming `loo` release. No version constraint on `loo`
  is added, since both output formats are supported.

* This change was coordinated with the `loo` maintainer, who reviewed the
  package during a reverse-dependency check and supplied the fix.

No changes were made to exported functions or the package API.

## R CMD check results

On a CRAN-like environment (without 'cmdstanr' installed):

0 errors | 0 warnings | 1 note

### NOTEs

* Package suggests `cmdstanr` which is available from r-universe
  (https://stan-dev.r-universe.dev), as specified in `Additional_repositories`
  in DESCRIPTION. `cmdstanr` is only needed at runtime for model fitting, not
  to install or check the package.

### Locally observed warnings/notes (environment-specific, not seen on CRAN)

When checked locally with 'cmdstanr' installed and an older local toolchain,
the following additional items appear. They are artifacts of the local
environment, not package defects:

* "Compilation used the following non-portable flag(s)"
  (`-Wno-deprecated-declarations`, `-Wno-ignored-attributes`,
  `-Wno-sign-compare`, `-Wno-tautological-compare`,
  `-Wno-unknown-warning-option`). These flags come from the Stan/CmdStan
  build system (via the 'instantiate' package) when Stan models are compiled.
  They are not controlled by this package. On CRAN, where 'cmdstanr' is not
  installed, model compilation is skipped and this warning does not occur.

* "A complete check needs the 'checkbashisms' script" and an HTML Tidy note
  ("'tidy' doesn't look like recent enough HTML Tidy") are due to tools
  missing/outdated on the local machine and are not package issues.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* local macOS, R 4.5.1, with the development version of `loo`
  (new data-frame `loo_compare()` output) to confirm forward compatibility
* GitHub Actions (ubuntu-latest): R release
* GitHub Actions (macOS-latest): R release
* GitHub Actions (windows-latest): R release
