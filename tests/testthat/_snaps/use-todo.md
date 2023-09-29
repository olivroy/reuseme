# Marking a TODO item as done works

    Code
      mark_todo_as_complete(line_id = 1, file = tmp, regexp = "I Want this done")
    Condition
      Error in `mark_todo_as_complete()`:
      x Cannot mark a TODO item as complete if it doesn't contain the tags.
      i Did not detect any "WORK", "FIXME", and "TODO" tags in the specified line.
    Code
      mark_todo_as_complete(line_id = 2, file = tmp)
    Condition
      Error in `mark_todo_as_complete()`:
      ! `regexp` must be a single string, not absent.

