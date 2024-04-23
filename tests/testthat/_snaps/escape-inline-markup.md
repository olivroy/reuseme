# escape_markup() works

    Code
      escape_markup("i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
    Output
      [1] "i{{gt_var}} in {{gt_var}} in gt_var in {.file {.url gt_var}}."
    Code
      escape_markup("{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
    Output
      [1] "{{gt_var}} in {{gt_var}} in gt_var in {.file {.url gt_var}}."

# replace_r_var() works

    Code
      replace_r_var("i{gt_var} in {{gt_var}} in gt_var in {.file {gt_var}}.")
    Output
      [1] "i{{gt_var}} in {{gt_var}} in gt_var in {.file {gt_var}}."

