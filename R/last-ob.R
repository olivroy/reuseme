carry_last_obs <- function(object) {
  # adapted from zoo::na.locf0()
  ok <- which(!is.na(object))
  if (is.na(object[1L])) {
    ok <- c(1L, ok)
  }

  gaps <- diff(c(ok, length(object) + 1L))
  object <- rep(object[ok], gaps)

  object
}
