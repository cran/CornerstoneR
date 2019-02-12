context("fitFunction")

test_that("Logistic", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "Logistic", resp.frml = "", preds.frml = "", start.vals = ""
                    , weights = "", algo.nls = "default"
                    )
  
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
                , lstScriptVars = scriptvars, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  # Group + Coeff * 2 + 7
  expect_data_frame(res$coeff, any.missing = FALSE, nrows = 2, ncols = 16, col.names = "named")
  expect_data_frame(res$predictions, any.missing = FALSE, nrows = 100, ncols = 4)
  expect_equal(res$predictions[, 1:2], dt[, 2:3])
  expect_set_equal(colnames(res$predictions), c("group1", "y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 2:4]), data.table(Coeff_a = c(1, 8), Coeff_b = c(10, 2), Coeff_c = c(1, -1)))
  expect_equal(round(res$coeff[, 5], digits = 1), data.table(Coeff_d = c(0.6, 0.3)))
  
  # error with algorithm 'plinear' -> empty coeff table
  scriptvars1 = scriptvars
  scriptvars1$algo.nls = "plinear"
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1", strGroups = "group1"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_data_frame(res$coeff[, -16], all.missing = TRUE)
  
  # one group
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  # Group + Coeff * 2 + 7
  expect_data_frame(res$coeff, any.missing = FALSE, nrows = 1, ncols = 15, col.names = "named")
  expect_data_frame(res$predictions, any.missing = FALSE, nrows = 100, ncols = 3)
  expect_equal(res$predictions[, 1], dt[, 2])
  expect_set_equal(colnames(res$predictions), c("y1", "Fitted", "Residuals"))
  expect_equal(round(res$coeff[, 1:3]), data.table(Coeff_a = 1, Coeff_b = 10, Coeff_c = 1))
  expect_equal(round(res$coeff[, 4], digits = 1), data.table(Coeff_d = 0.6))
  
  # no predictor
  createCSEnvir(dt[, "y1"], strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  expect_error(fitFunction())
  
  # singular gradient
  set.seed(1620)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 2, 1, 0.1, 1)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_scalar_na(res$coeff[, Iterations])
  expect_scalar_na(res$coeff[, StopCode])
  expect_equal(dim(res$predictions), c(100, 3))
  
  # below "minFactor"
  set.seed(1621)
  dt = data.table(x1 = sample(seq(-10, 10, length.out = 100)))
  dt[, y1 := fun(x1, 1, 2, 1, 0.1, 0.4)]
  # create CS-R output
  createCSEnvir(dt, strPreds = "x1", strResps = "y1"
                , lstScriptVars = scriptvars, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_scalar_na(res$coeff[, StopCode])
})

test_that("User Definded", {
  set.seed(1619)
  env = globalenv()
  
  # default settings for script variables
  scriptvars = list(math.fun = "User Defined", preds.frml = "", resp.frml = ""
                    , start.vals = "", weights = "", algo.nls = "default"
                    )
  
  # first standard test
  # set script variables
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a + b*log(Girth) + c*log(Height)"
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$start.vals = "a=1, b=1, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run fit
  expect_true(fitFunction())
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 2)
  # Group + Coeff * 2 + 7
  expect_data_frame(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_frame(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  expect_numeric(unlist(res$coeff[, 1]), lower = -7, upper = -6)
  expect_numeric(unlist(res$coeff[, 2:3]), lower = 1, upper = 2)
  expect_numeric(unlist(res$coeff[, 4:8]), lower = 0, upper = 1)
  
  # two groups
  trees.grp = trees
  trees.grp$grouping = sample(c("A", "B"), size = nrow(trees.grp), replace = TRUE)
  # create CS-R output
  createCSEnvir(trees.grp, strPreds = c("Height", "Girth"), strResps = "Volume", strGroups = "grouping"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_list(res, len = 2)
  expect_true(all(res$coeff$StopCode == 0))
  # Group + Coeff * 2 + 7
  expect_data_frame(res$coeff, any.missing = FALSE, nrows = 2, ncols = 14, col.names = "named")
  expect_data_frame(res$predictions, any.missing = FALSE, nrows = 31, ncols = 4)
  
  # weighting
  trees.grp = trees
  trees.grp$Weighting = 1
  scriptvars1$weights = "Weighting"
  # create CS-R output
  createCSEnvir(trees.grp, strPreds = c("Height", "Girth"), strResps = "Volume", strAuxs = "Weighting"
                , lstScriptVars = scriptvars1, env = env
                )
  # run fit
  res = fitFunction(return.results = TRUE)
  expect_equal(res$coeff$StopCode, 0)
  # Group + Coeff * 2 + 7
  expect_data_frame(res$coeff, any.missing = FALSE, nrows = 1, ncols = 13, col.names = "named")
  expect_data_frame(res$predictions, any.missing = FALSE, nrows = 31, ncols = 3)
  
  # missing start values
  scriptvars1 = scriptvars
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$preds.frml = "a + b*Girth"
  # create CS-R output
  createCSEnvir(trees[, c("Girth", "Volume")], strResps = "Volume", strPreds = "Girth"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
  
  # no predictors
  scriptvars1 = scriptvars
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$preds.frml = "a"
  scriptvars1$start.vals = "a = 1"
  # create CS-R output
  createCSEnvir(trees[, "Volume", drop = FALSE], strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
  
  # malformed start values
  scriptvars1 = scriptvars
  scriptvars1$preds.frml = "a + b*log(Girth) + c*log(Height)"
  scriptvars1$resp.frml = "log(Volume)"
  scriptvars1$start.vals = "a=1, =1, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())

  # malformed start values
  scriptvars1$start.vals = "a=1, b=x, c=1"
  # create CS-R output
  createCSEnvir(trees, strPreds = c("Height", "Girth"), strResps = "Volume"
                , lstScriptVars = scriptvars1, env = env
                )
  expect_error(fitFunction())
})
