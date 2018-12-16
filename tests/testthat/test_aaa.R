context("Init")

test_that("Init", {
  # due to notes about cs.session.test in R CMD check
  # initialise 'cs.session.test' environment before calling createCSFunctions()
  env = globalenv()
  createCSEnvir(carstats[, .(Cylinders, Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower", strGroups = "Cylinders"
                , env = env
  )
  expect_environment(cs.session.test)

  # init cs.* functions after create cs.session.test environment
  createCSFunctions(env = env)
})