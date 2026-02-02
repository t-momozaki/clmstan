# Get supported link functions

Get supported link functions

## Usage

``` r
supported_links(type = c("all", "standard", "flexible"))
```

## Arguments

- type:

  Character string specifying which links to return:

  - `"all"` (default): All supported link functions

  - `"standard"`: Standard links without additional parameters

  - `"flexible"`: Flexible links with additional parameters

## Value

A character vector of supported link function names

## Examples

``` r
supported_links()
#>  [1] "logit"        "probit"       "cloglog"      "loglog"       "cauchit"     
#>  [6] "tlink"        "aranda_ordaz" "gev"          "sp"           "log_gamma"   
#> [11] "aep"         
supported_links("standard")
#> [1] "logit"   "probit"  "cloglog" "loglog"  "cauchit"
supported_links("flexible")
#> [1] "tlink"        "aranda_ordaz" "gev"          "sp"           "log_gamma"   
#> [6] "aep"         
```
