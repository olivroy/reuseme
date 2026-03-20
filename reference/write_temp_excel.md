# Write temporary Excel to Downloads

Write temporary Excel to Downloads

## Usage

``` r
write_temp_excel(dat, name, font = "Arial", na = "", dir = NULL, mip = NULL)
```

## Arguments

- dat:

  data frame

- name:

  name of file

- font:

  font name

- na:

  passed to
  [`openxlsx2::wb_add_data()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_data.html)

- dir:

  A directory to override.

- mip:

  privacy string

## Value

A new Excel file
