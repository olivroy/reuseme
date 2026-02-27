# Write temporary Excel to Downloads

Write temporary Excel to Downloads

## Usage

``` r
write_temp_excel(dat, name, font = "Arial", na = "")
```

## Arguments

- dat:

  data frame

- name:

  name of file

- font:

  font name

- na:

  passed to `openxslx2::wb_add_data()`

## Value

A new Excel file
