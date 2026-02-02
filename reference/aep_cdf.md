# Asymmetric Exponential Power (AEP) link CDF

The AEP distribution with alpha = 0.5 (fixed for identifiability).

## Usage

``` r
aep_cdf(x, theta1, theta2)
```

## Arguments

- x:

  Numeric vector

- theta1:

  Left tail parameter (theta1 \> 0)

- theta2:

  Right tail parameter (theta2 \> 0)

## Value

CDF values

## Details

For x \<= 0: F(x) = 0.5 \* (1 - P(1/theta1, u1)) where u1 = (\|x\| \* 2
\* Gamma(1 + 1/theta1))^theta1

For x \> 0: F(x) = 0.5 + 0.5 \* P(1/theta2, u2) where u2 = (x \* 2 \*
Gamma(1 + 1/theta2))^theta2

P(a, x) is the regularized incomplete gamma function (pgamma).

Special case: theta1 = theta2 gives a symmetric distribution. Note:
theta = 2 has a Gaussian kernel but is NOT equal to probit due to
scaling.

Reference: Naranjo et al. (2015) Statistics and Computing
