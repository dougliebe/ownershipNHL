
library(coach)
add_stack_size_constraint()
create_model <- function(test_data) {
  model <- model_generic(test_data,total_salary = 50000,
                         roster_size = 9,
                         max_from_team = 5)
  constraints <- list(
    "C" = 2,
    "W" = 3,
    "D" = 2,
    "G" = 1,
    "D/W/C" = 1
  )
  model <- add_generic_positions_constraint(model, test_data, constraints)
  model <- coach::add_stack_size_constraint()
}

