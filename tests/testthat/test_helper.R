context("Helper")

test_that("Helper", {
  env = globalenv()
  
  createCSEnvir(carstats[, .(Cylinders, Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower", strGroups = "Cylinders"
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  res = cs.in.dataset()
  expect_data_table(res, nrows = 406, ncols = 3)
  
  createCSEnvir(carstats[, .(Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower"
                , lstScriptVars = list(a = 4, b = "Text")
                , env = env
                )
  expect_list(cs.in.scriptvars())
  expect_equal(cs.in.scriptvars("a"), 4)
  expect_equal(cs.in.scriptvars("b"), "Text")
})
