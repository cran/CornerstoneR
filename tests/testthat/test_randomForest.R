context("Random Forest")

test_that("Classification", {
  set.seed(1619)
  env = globalenv()
  
  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = NULL, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run randomForest
  res = randomForest(return.results = TRUE)
  expect_equal(res$statistics[-9, 2]
               , data.table(Value = c("Classification", "500", "150", "4", "2", "1", "permutation", "4.667"))
               )
  expect_data_table(res$importances, any.missing = FALSE, nrows = 4, ncols = 2)
  expect_numeric(res$importances$Importance, lower = 0, upper = 0.5)
  expect_set_equal(res$importances$Variable, c("Petal.Width", "Petal.Length", "Sepal.Length", "Sepal.Width"))
  expect_equal(res$predictions$Species, iris$Species)
  expect_equal(sum(res$predictions$Obs.Species), 150)
  expect_equal(sum(res$predictions$Resid.Species), 0)
  expect_equal(res$confusions$Species$N, c(rep(50, 3), rep(0, 6)))
  
  # different script variables
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = FALSE, use.rows = "unbrushed", num.trees = 200)
                , env = env
                )
  res = randomForest(return.results = TRUE)
  expect_equal(res$statistics[-9, 2]
               , data.table(Value = c("Classification", "200", "50", "3", "1", "1", "permutation", "6"))
               )
  
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = FALSE, use.rows = "brushed", num.trees = 200)
                , env = env
                )
  res = randomForest(return.results = TRUE)
  expect_equal(res$statistics[-9, 2]
               , data.table(Value = c("Classification", "200", "100", "3", "1", "1", "permutation", "10"))
               )
  
  set.seed(1619)
  createCSEnvir(iris[, c(1:3, 5)], blnBrush = sample(c(!logical(100), logical(50)))
                , strPreds = colnames(iris)[1:3], strResps = colnames(iris)[5]
                , lstScriptVars = list(brush.pred = TRUE, use.rows = "brushed", num.trees = 200)
                , env = env
                )
  res = randomForest(return.results = TRUE)
  expect_equal(res$statistics[-9, 2]
               , data.table(Value = c("Classification", "200", "150", "4", "2", "1", "permutation", "7.333"))
               )
  
  # predict missing
  set.seed(1619)
  iris.miss.resp = iris
  iris.miss.resp$Species[sample(c(!logical(100), logical(50)))] = NA
  createCSEnvir(iris.miss.resp
                , strPreds = colnames(iris)[1:4], strResps = colnames(iris)[5]
                , lstScriptVars = NULL
                , env = env
                )
  res = randomForest(return.results = TRUE)
  expect_equal(sum(is.na(res$predictions$Resid.Species)), 100)
})

test_that("Regression", {
  set.seed(1619)
  env = globalenv()
  
  # create CS-R output
  createCSEnvir(warpbreaks, strPreds = colnames(warpbreaks)[2:3], strResps = colnames(warpbreaks)[1]
                , lstScriptVars = NULL, env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  
  # run randomForest
  res = randomForest(return.results = TRUE)
  expect_equal(res$statistics[-10, 2]
               , data.table(Value = c("Regression", "500", "54", "2", "1", "5", "permutation", "131.7", "0.2438"))
               )
  expect_data_table(res$importances, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_equal(res$importances$Variable, c("tension", "wool"))
  expect_numeric(res$importances$Importance, lower = 15, upper = 65)
  expect_equal(sum(res$predictions$Obs.breaks), 54)
  expect_equal(res$predictions$breaks, warpbreaks$breaks)
  expect_list(res$confusions, len = 0)

  # predict missing
  set.seed(1619)
  carstats.miss.resp = carstats
  carstats.miss.resp$Displacement[sample(c(!logical(300), logical(106)))] = NA
  createCSEnvir(carstats.miss.resp[, c("Displacement", "Weight", "Acceleration")]
                , strPreds = c("Weight", "Acceleration")
                , strResps = "Displacement"
                , lstScriptVars = NULL, env = env
                )
  res = randomForest(return.results = TRUE)
  expect_equal(sum(is.na(res$predictions$Resid.Displacement)), 300)
})

test_that("Multi", {
  set.seed(1619)
  env = globalenv()
  
  # create CS-R output
  createCSEnvir(iris, strPreds = colnames(iris)[1:3], strResps = colnames(iris)[4:5]
                , lstScriptVars = NULL, env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  
  # run randomForest
  res = randomForest(return.results = TRUE)
  expect_data_table(res$statistics, nrows = 2, ncols = 12)
  expect_equal(colnames(res$statistics)[1], "Response")
  expect_data_table(res$importances, nrows = 2, ncols = 4)
  expect_equal(colnames(res$importances)[1], "Response")
  expect_data_table(res$predictions, nrows = 150, ncols = 8)
  expect_list(res$confusions, len = 1)
})
