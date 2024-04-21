## code to prepare datasets goes here


simulation_data_two_groups <- run_simulations(
  sample_size = 30:200,
  sample_prob = c(0.5,0.5),
  prob0 = c(0.25,0.25,0.25,0.25),
  prob1 = c(0.4,0.3,0.2,0.1),
  niter = 200
) %>%
  bind_rows()


usethis::use_data(simulation_data_two_groups, overwrite = TRUE)



# set probabilities equal to make data for checking Type 1 Error

simulation_data_one_group <- run_simulations(
  sample_size = 30:200,
  sample_prob = c(0.5,0.5),
  prob0 = c(0.25,0.25,0.25,0.25),
  prob1 = c(0.25,0.25,0.25,0.25),
  niter = 200
) %>%
  bind_rows()


usethis::use_data(simulation_data_one_group, overwrite = TRUE)


