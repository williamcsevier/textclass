test_that("sparse warnings",{
  expect_error(tidyDTM(AFICAdata, 0))
  expect_error(tidyDTM(AFICAdata, 1))
  expect_error(tidyDTM(AFICAdata, h))
})
