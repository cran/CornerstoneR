context("Reshape")

test_that("MeltLong", {
  set.seed(97)
  
  env = globalenv()
  
  dtTest = data.table(
    i_1 = c(1:4, NA, 5)
    , i_2 = c(51, 61, NA , 71, 81, 91)
    , f1 = factor(sample(c(letters[1:3], NA), 6, TRUE))
    , f2 = factor(c("z", "a", "x", "c", "x", "x"), ordered = TRUE)
    , r_1 = rnorm(6)
    , r_2 = rexp(6)
    , r3 = rweibull(6, shape = 2)
  )
  
  # two without splitting
  createCSEnvir(dtTest[, c(1:4)]
                , strPreds = c("i_1", "i_2"), strResps = c("f1", "f2")
                , lstScriptVars = list(split = "_")
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 4)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable", "value"))
  # splitting response names
  createCSEnvir(dtTest[, c(1, 2, 5, 6)]
                , strPreds = c("i_1", "i_2"), strResps = c("r_1", "r_2")
                , lstScriptVars = list(split = "_")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 12, ncols = 5)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable1", "variable2", "value"))
  # mixed response types
  createCSEnvir(dtTest[, 1:6]
                , strPreds = c("i_1", "i_2"), strResps = colnames(dtTest)[3:6]
                , lstScriptVars = list(split = "_")
                , env = env
                )
  expect_warning(reshapeLong())
  # mixed response names
  createCSEnvir(dtTest[, -c(3:4)]
                , strPreds = c("i_1", "i_2"), strResps = colnames(dtTest)[5:7]
                , lstScriptVars = list(split = "_")
                , env = env
                )
  res = reshapeLong(return.results = TRUE)
  expect_data_table(res$reshapeLong, nrows = 18, ncols = 5)
  expect_equal(colnames(res$reshapeLong), c("i_1", "i_2", "variable1", "variable2", "value"))
})

test_that("CastWide", {
  env = globalenv()
  
  # single
  createCSEnvir(Indometh
                , strGroups = colnames(Indometh)[1], strPreds = colnames(Indometh)[2]
                , strResps = colnames(Indometh)[3]
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(reshapeWide())
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 6, ncols = 12)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 12)], c("Subject", "conc_0.25", "conc_8"))
  
  # additional response
  IndoExt = cbind(Indometh, rnd = rnorm(66))
  createCSEnvir(IndoExt
                , strGroups = colnames(IndoExt)[1], strPreds = colnames(IndoExt)[2]
                , strResps = colnames(IndoExt)[3:4]
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 6, ncols = 23)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 12, 13, 23)], c("Subject", "conc_0.25", "conc_8", "rnd_0.25", "rnd_8"))
  
  # multiple groups
  DT = data.table(v1 = rep(1:2, each = 6), v2 = rep(rep(1:3, 2), each = 2), v3 = rep(1:2, 6), v4 = rnorm(6))
  createCSEnvir(DT
                , strGroups = c("v1", "v2"), strPreds = "v3"
                , strResps = "v4"
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 6, ncols = 4)
  expect_equal(colnames(res$reshapeWide), c("v1", "v2", "v4_1", "v4_2"))
  
  # multiple predictors
  createCSEnvir(DT
                , strGroups = "v1", strPreds = c("v2", "v3")
                , strResps = "v4"
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 2, ncols = 7)
  expect_equal(colnames(res$reshapeWide)[c(1, 2, 7)], c("v1", "v4_1_1", "v4_3_2"))
  
  # carstats 1
  createCSEnvir(carstats[, .(MPG, Cylinders, Model.Year)]
                , strPreds = "Cylinders", strResps = "MPG", strGroups = "Model.Year"
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  expect_message(reshapeWide(return.results = TRUE), "Aggregate function missing")
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 13, ncols = 6)
  # carstats 2
  createCSEnvir(carstats[, .(Cylinders, Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower", strGroups = "Cylinders"
                , lstScriptVars = list(nodrop = FALSE)
                , env = env
                )
  expect_message(reshapeWide(return.results = TRUE), "Aggregate function missing")
  res = reshapeWide(return.results = TRUE)
  expect_data_table(res$reshapeWide, nrows = 5, ncols = 84)
})
