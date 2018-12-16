context("MosaicPlot")

test_that("MosaicPlot", {
  env = globalenv()
  createCSEnvir(as.data.frame(Titanic)
                , strPreds = c("Class", "Sex", "Age", "Survived"), strResps = "Freq"
                , env = env
  )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  expect_true(mosaicPlot())
})
