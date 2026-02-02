# Get link type number for Stan

Converts a link function name to the integer code used in Stan.

## Usage

``` r
get_link_type(link)
```

## Arguments

- link:

  A character string specifying the link function

## Value

An integer (1-11) representing the link type

## Details

Link type mapping:

- 1: logit

- 2: probit

- 3: cloglog

- 4: loglog

- 5: cauchit

- 6: tlink

- 7: aranda_ordaz

- 8: sp

- 9: log_gamma

- 10: gev

- 11: aep
