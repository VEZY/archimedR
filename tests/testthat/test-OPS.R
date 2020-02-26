context("OPS")

test_that("Read OPS", {
  OPS= read_ops(file = "data/example.ops")
  ref_plant=
    data.frame(
      sceneId= c(1,1), plantId= c(1,2), plantFileName= rep("opf/DA1_Average_MAP_36.opf",2),
      x=c(2.3,6.9), y= c(3.99,11.96), z= c(0,0), scale= c(1,1), inclinationAzimut= c(0,0),
      inclinationAngle= c(0,0), stemTwist= c(0,0), Functional_group= c("two","two"))
  expect_true(all(OPS$dimensions== c(0,0,0,9.21,15.952,"flat",146.91792)))
  expect_true(all(OPS$plants == ref_plant))
  expect_true(all(OPS$chaining == c(-1,1,1)))
})

test_that("Write OPS", {
  OPS= read_ops(file = "data/example.ops")
  write_ops(data = OPS, file = "data/example_new.ops")
  OPS_new= read_ops(file = "data/example_new.ops")
  expect_equal(OPS_new,OPS)
  unlink("data/example_new.ops", force=TRUE)
})

