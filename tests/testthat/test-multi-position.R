context("multi-position")

# test data
n <- 4
data <- data.frame(
  player = c("alice", "bob", "carol", "dan"),
  team = c("alpha", "beta", "gamma", "gamma"),
  opp_team = c("beta", "alpha", "omega", "omega"),
  position = c("P", "3B/OF", "2B", "3B"),
  fpts_proj = c(30, 20, 10, 5),
  salary = rep(33L, n),
  row_id = seq_len(n),
  player_id = as.character(seq_len(n)),
  stringsAsFactors = FALSE)

test_that("draftkings multi-position eligibility works", {
  # data needs to have one position per player
  # separate if not the case
  data[["position"]] <- strsplit(data[["position"]], "/")
  data <- unnest_col(data, "position")
  data[["row_id"]] <- seq_len(nrow(data))

  model <- model_generic(data, total_salary = 100, roster_size = 3) %>%
    add_unique_id_constraint(data)


  results <- optimize_generic_one(data, model)[["roster"]]
  expect_equal(results[["player"]], c("alice", "bob", "carol"))
})
