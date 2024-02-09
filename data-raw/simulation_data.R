## code to prepare datasets goes here


simulation_data_two_groups <- run_simulations(
  sample_size = 200,
  sample_prob = c(0.5,0.5),
  prob0 = c(0.25,0.25,0.25,0.25),
  prob1 = c(0.4,0.3,0.2,0.1),
  niter = 200
)


simulation_data_two_groups_formatted <- simulation_data_two_groups %>%
  format_simulation_data()

usethis::use_data(simulation_data_two_groups, overwrite = TRUE)
usethis::use_data(simulation_data_two_groups_formatted, overwrite = TRUE)





# set probabilities equal to make data for checking Type 1 Error

simulation_data_one_group <- run_simulations(
  sample_size = 200,
  sample_prob = c(0.5,0.5),
  prob0 = c(0.25,0.25,0.25,0.25),
  prob1 = c(0.25,0.25,0.25,0.25),
  niter = 200
)

simulation_data_one_group_formatted <- simulation_data_one_group %>%
  format_simulation_data()

usethis::use_data(simulation_data_one_group, overwrite = TRUE)
usethis::use_data(simulation_data_one_group_formatted, overwrite = TRUE)


