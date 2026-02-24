# Parse Ratio Text

This function parses text from ratios which are written in the format of
1-2 digit numbers separated by a colon and trailing with another 1-2
digit number. The text is processed into a numeric vector of length 2
containing the two numbers.

## Usage

``` r
parse_ratio_text(text)
```

## Arguments

- text:

  A string of in the form of e.g. 5:95 or 70:30

## Value

Numeric vector of length 2

## Examples

``` r
parse_ratio_text("70:30")
#> [1] 0.7 0.3
```
