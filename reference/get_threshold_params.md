# Get threshold parameter information

Get threshold parameter information

## Usage

``` r
get_threshold_params(threshold)
```

## Arguments

- threshold:

  A character string specifying the threshold structure

## Value

A list with parameter names and their Stan types

## Details

Threshold parameters:

- `flexible`: `c` (ordered\[K-1\])

- `equidistant`: `c1` (real), `d` (real\<lower=0\>)

- `symmetric`: `c_pos` (positive_ordered\[(K-1)/2\])
