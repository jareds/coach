#' @rdname model_generic
#' @export
model_dk_nfl_showdown <- function(data, existing_rosters = list()) {
  # params
  total_salary <- 50E3
  roster_size <- 6L
  max_from_team <- 5L

  # build model
  model_generic(data, total_salary, roster_size, max_from_team, existing_rosters) %>%
    add_dk_nfl_showdown_roster_positions_constraint(data)
}

#' @importFrom ompr add_constraint sum_expr
#' @keywords internal
add_dk_nfl_showdown_roster_positions_constraint <- function(model, nfl) {
  # position constraint helpers
  n <- nrow(nfl)
  is_position <- function(pos) {
    function(i) {
      as.integer(pos == nfl$position[i])
    }
  }

  CPT <- is_position("CPT")
  FLEX <- is_position("FLEX")

  model %>%
    # cpt
    add_constraint(sum_expr(colwise(CPT(i)) * x[i], i = 1:n) == 1) %>%
    # flex
    add_constraint(sum_expr(colwise(FLEX(i)) * x[i], i = 1:n) >= 5) 

}

