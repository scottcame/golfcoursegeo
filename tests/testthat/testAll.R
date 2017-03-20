# package tests

library(testthat)
library(golfcoursegeo)

kmlFile <- 'Jumeirah Golf Estates.kml'

test_that('basic layer works', {
    df <- extractHoleInfoFromKmlLayer(layerName='Hole 1', kmlFile=kmlFile)
    expect_equal(nrow(df), 1) # to do, make this a little more robust
})

test_that('missing Tee errs out', {
  expect_error(extractHoleInfoFromKmlLayer(layerName='Hole 2', kmlFile=kmlFile), regexp='.+Tee.+')
})

test_that('missing Green errs out', {
  expect_error(extractHoleInfoFromKmlLayer(layerName='Hole 3', kmlFile=kmlFile), regexp='.+Green.+')
})

test_that('out of order width lines ok', {
  df <- extractHoleInfoFromKmlLayer(layerName='Hole 5', kmlFile=kmlFile)
  expect_equal(df$Width200, 32.06, tolerance=.01)
  expect_equal(df$Width250, 39.59, tolerance=.01)
  expect_equal(df$Width300, 40.36, tolerance=.01)
})