# Set working directory to project root 
# (only needed if called by test_dir())
setwd("../..")

# Only test this file
# test_file("tests/testthat/test-unit-dtree.R")

test_that("Unique ancestors within deviant groups are found correctly", {
  # Example with deviant group nodes one of whose immediate children is also deviants group node
  parent_ids1 <- c("4", "40", "81", "12", "24", "119","239","31")
  actual_ancestors1 <- c("4", "40","12", "119", "31")
  
  test_ancestors1 <- main_ancestors(parent_ids1)
  expect_true(all(test_ancestors1 == actual_ancestors1))
  
  # Example with deviant group node whose immediate children nodes are not deviants groups
  # (i.e., 29 --> not 58, 59 --> 117 --> not 234, 235 --> 469)
  parent_ids2 <- c("2", "12","24", "204","26", "29", "117", "469","15", "31","62", "124","63")
  actual_ancestors2 <- c("2", "12", "26", "29", "15")
  
  test_ancestors1 <- main_ancestors(parent_ids1)
  expect_true(all(test_ancestors1 == actual_ancestors1))
})

test_that("Paths from root to nodes are computed correctly", {
  # Even numbered node
  expect_true(all(path_to(468) == c(1, 3, 7, 14, 29, 58, 117, 234, 468)))
  
  # Odd numbered node
  expect_true(all(path_to(469) == c(1, 3, 7, 14, 29, 58, 117, 234, 469)))
})

test_that("All leaves ultimately belonging to a node are correctly found", {
  dtree <- readRDS(file = "tests/fixtures/utaut-dtree.rds")
  leaf_ids <- row.names(subset(dtree$tree$frame, var=="<leaf>"))
  
  leaves_of_2     <- leaves_from_nodes(parent_ids = 2, leaf_ids = leaf_ids)
  leaves_of_234   <- leaves_from_nodes(parent_ids = 234, leaf_ids = leaf_ids)
  leaves_of_469   <- leaves_from_nodes(parent_ids = 469, leaf_ids = leaf_ids)
  leaves_of_2_469 <- leaves_from_nodes(parent_ids = c(2,469), leaf_ids = leaf_ids)
  
  # Make sure correct leaves are found
  expect_equal(leaves_of_234[[1]], c("468", "938", "939"))
  expect_equal(leaves_of_469[[1]], c("938", "939"))
  
  # Make sure right number of leaves are found
  expect_equal(length(leaves_of_2[[1]]), 73)
  expect_equal(length(leaves_of_469[[1]]), 2)
  
  # Make sure right number of leaves of multiple nodes are found
  expect_equal(length(unlist(leaves_of_2_469)), 75)  # node 469 is not under node 2; distinct leaves
})