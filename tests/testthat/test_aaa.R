context("Initial Tests")

test_that("Invoke Local Interface from R", {
  expect_true(invokeFromR())
  expect_null(cs.in.auxiliaries())
  expect_null(cs.in.brushed())
  expect_null(cs.in.dataset())
  expect_null(cs.in.excluded())
  expect_null(cs.in.groupvars())
  expect_null(cs.in.predictors())
  expect_null(cs.in.responses())
  expect_null(cs.in.scriptvars())
  expect_null(cs.in.subsets())
  expect_null(cs.in.subsets.current())
  expect_null(cs.quote())
  expect_null(cs.out.dataset())
  expect_null(cs.out.emf())
  expect_null(cs.out.png())
})
