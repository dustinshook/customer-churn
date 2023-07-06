library(faux)

existing_customers <-
    readr::read_csv('data/WA_Fn-UseC_-Telco-Customer-Churn.csv')

randomly_generated_customers <- 
    sim_joint_dist(existing_customers, n = 5)

randomly_generated_customers <- 
    sim_mixed_df(existing_customers, sub_n = NULL, item_n = NULL, dv = "customerID")
