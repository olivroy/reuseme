# Marking a TODO item as done works

    Code
      complete_todo(line = 1, file = tmp, regexp = "I Want this done")
    Condition
      Error in `complete_todo()`:
      x Could not delete the TODO item.
      i Line 1 does not contain any "WORK", "FIXME", or "TODO" tags.
    Code
      complete_todo(line = 2, file = tmp)
    Condition
      Error in `complete_todo()`:
      ! `regexp` must be a single string, not absent.

