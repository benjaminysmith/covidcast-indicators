## Output formatting and reporting.

Reporter <- setRefClass(
  "Reporter",
  fields = list(num_bad = "numeric"),
  methods = list(
    report = function(bad_vec, filename, reason) {
      if (any(bad_vec)) {
        num_bad <<- num_bad + 1
      }

      ## TODO Implement some nice reporting mechanism.
    }
  ))
