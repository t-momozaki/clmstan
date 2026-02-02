# Validate threshold specification

Validate threshold specification

## Usage

``` r
validate_threshold(threshold, K = NULL)
```

## Arguments

- threshold:

  A character string specifying the threshold structure

- K:

  Number of ordinal categories (optional)

## Value

TRUE if valid, otherwise throws an error

## Details

Validation rules:

- `flexible`: No restrictions

- `equidistant`: K = 2 triggers a warning (d is not identifiable)

- `symmetric`: Requires K \>= 3
