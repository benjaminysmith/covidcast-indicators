
context("Sanity checking methods")

test_that("bad geo_ids are rejected", {

  counties <- data.frame(geo_id = c("00101", "1", "98765", "ducks", "123432"))
  expect_equal(check_geo_ids(counties, "county"),
               c("1", "ducks", "123432"))

  ## TODO Test format rules for HRR, MSA, DMA, states
})
