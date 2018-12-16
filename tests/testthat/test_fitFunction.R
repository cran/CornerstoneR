context("fitFunction")

test_that("Logistic", {
  set.seed(1619)
  env = globalenv()
  
  # logistic function
  fun = function(x, a, b, c, d, sigma = 1) {
    a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
  }
  
  # two groups
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
                  , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
                  )
  dt[group1 == "A", y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1"
                , lstScriptVars = list(math.fun = "Logistic"), env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_equal(res$predictions[, 1:2], dt[, 2:3])
  expect_equal(dim(res$coeff), c(2, 14))
  expect_equal(dim(res$predictions), c(100, 4))
  expect_equal(colnames(res$predictions), c("group1", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 2:4]), data.table(Coeff_a = c(1, 8), Coeff_b = c(10, 2), Coeff_c = c(1, -1)))
  expect_equal(round(res$coeff[, 5], digits = 1), data.table(Coeff_d = c(0.6, 0.3)))
  
  # one group
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = list(math.fun = "Logistic"), env = env
                )
  
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_equal(res$predictions[, 1], dt[, 2])
  expect_equal(dim(res$coeff), c(1, 13))
  expect_equal(dim(res$predictions), c(100, 3))
  expect_equal(colnames(res$predictions), c("y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 1:3]), data.table(Coeff_a = 1, Coeff_b = 10, Coeff_c = 1))
  expect_equal(round(res$coeff[, 4], digits = 1), data.table(Coeff_d = 0.6))
  
  # singular gradient
  set.seed(1620)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 2, 1, 0.1, 1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = list(math.fun = "Logistic"), env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_scalar_na(res$coeff[, Iterations])
  expect_equal(res$coeff[, Message], "singular gradient")
  expect_equal(dim(res$predictions), c(100, 3))
  # two groups
  dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
                    , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
                    )
  dt[group1 == "A", y1 := fun(x1, 1, 2, 1, 0.1, 1)]
  dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1"
                , lstScriptVars = list(math.fun = "Logistic"), env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_equal(res$coeff[, Iterations], c(NA, 7))
  expect_equal(res$coeff[, Message], c("singular gradient", "converged"))
  expect_equal(dim(res$predictions), c(100, 4))
  
  # below "minFactor"
  set.seed(1621)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 2, 1, 0.1, 0.4)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = list(math.fun = "Logistic"), env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_match(res$coeff[, Message], "step factor \\d*\\.\\d* reduced below 'minFactor'")
})
