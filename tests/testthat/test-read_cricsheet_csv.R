test_that("specific CSVs work with read_cricsheet_csv()", {
  expect_is(read_cricsheet_csv(path = "211028.csv"), "tbl_df")
  expect_is(read_cricsheet_csv(path = "291352.csv"), "tbl_df")
})

