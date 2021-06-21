# Set working directory to project root 
# (only needed if called by test_dir())
setwd("../..")

# Only test this file
# test_active_file("tests/testthat/test-unit-dtree.R")

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
  
  leaves_of_1     <- leaves_from_nodes(parent_ids = 1, leaf_ids = leaf_ids)
  leaves_of_2     <- leaves_from_nodes(parent_ids = 2, leaf_ids = leaf_ids)
  leaves_of_3     <- leaves_from_nodes(parent_ids = 3, leaf_ids = leaf_ids)
  leaves_of_152   <- leaves_from_nodes(parent_ids = 152, leaf_ids = leaf_ids)
  leaves_of_2_1536 <- leaves_from_nodes(parent_ids = c(2,1536), leaf_ids = leaf_ids)
  
  # Make sure correct leaves are found
  expect_equal(leaves_of_152[[1]], c("304", "305"))
  
  # Make sure right number of leaves are found
  expect_equal(length(leaves_of_1[[1]]), 216)
  expect_equal(length(leaves_of_1[[1]]), length(leaf_ids))
  expect_equal(length(leaves_of_1[[1]]), 
               length(leaves_of_2[[1]]) + length(leaves_of_3[[1]]))
  
  # Make sure right number of leaves of multiple nodes are found
  expect_equal(length(unlist(leaves_of_2_1536)), 152)  # node 1536 is not under node 2; distinct leaves
})

tol <- 1e-10
dtree <- readRDS(file = "tests/fixtures/utaut-dtree.rds")

test_that("Correct group rules are extracted", {
  rules <- group_rules('a', dtree)
  expect_equal(unname(unlist(rules$construct)), 
               c("BI", "FC", "HAB"))
  expect_equal(unname(unlist(rules$gte)) - c(NA, -2.4459581,  0.9988594) < tol, 
               c(NA, TRUE, TRUE))
})

test_that("Correct node competes are extracted", {
  group_names <- names(dtree$deviant_groups)
  group_nodes <- dtree$group_roots[group_names]
  correct_competes <- readRDS("tests/fixtures/utaut-dtree-competes-a.rds")
  compete_rules <- competes(group_nodes[['a']], dtree)
  # saveRDS(compete_rules, "tests/fixtures/utaut-dtree-competes-a.rds")
  
  expect_equal(all(compete_rules == correct_competes), TRUE)
})