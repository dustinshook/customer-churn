library(tidyverse)
library(lubridate)

xgb_mod <- 
    readr::read_rds('trained_models/xgboost_racing_tuned.rds')

glm_mod <-
    readr::read_rds('trained_models/lasso_penalty_tuned.rds')

get_telco_data <- function(raw = FALSE) {
    data <- readr::read_csv('data/WA_Fn-UseC_-Telco-Customer-Churn.csv')
    
    if (raw) {
        return(data)
    } else {
        data %>%
            parse_telco_data() %>%
            return()
    }
}

parse_telco_data <- function(.data) {
    parsed_data <-
        .data %>%
        mutate(
            across(
                c(
                    # re code the following categorical predictors
                    DeviceProtection,
                    OnlineBackup,
                    StreamingTV,
                    TechSupport,
                    StreamingMovies,
                    OnlineSecurity
                    # if the factor level is 'no internet service' replace with a simple 'no'
                ),
                ~ if_else(.x == 'No internet service', 'No', .x)
            ),
            # do the same for the multi phone lines column for consistency
            MultipleLines = if_else(MultipleLines == 'No phone service', 'No', MultipleLines),
            # re code the Senior Citizen column for consistency
            SeniorCitizen = case_match(SeniorCitizen,
                                       1 ~ 'Yes',
                                       0 ~ 'No',
                                       .default = 'No')
        ) %>%
        select(-gender) %>%
        mutate(across(where(is.character), as.factor),
               customerID = as.character(customerID))
    
}

random_id <- function() {
    numbers = 1:9
    id = c(
        sample(LETTERS, 3, replace = T),
        sample(numbers, 3, replace = T),
        sample(LETTERS, 2, replace = T)
    )
    
    return(paste0(id, collapse = ""))
}

calc_customer_tenure_stats <-
    function(tenure, totalCharges) {
        id = random_id()
        charge = totalCharges / tenure
        
        tibble(amount = rep(charge, tenure),
               month = 1:tenure) %>%
            mutate(
                invoice_id = str_c(id, month, sep = "-"),
                month = now() %m-% months(month)
            ) %>%
            arrange(month) %>%
            mutate(charges = cumsum(amount))
    }

renderProductList <-
    function(data) {
        products <-
            data %>%
            select(
                PhoneService,
                InternetService,
                Contract,
                PaymentMethod
            ) %>%
            pivot_longer(everything())
    }

reframeProducts <-
    function(data) {
        tibble(
            products = case_when()
        )
    }

# data <-
#     get_telco_data() %>%
#     filter(customerID == "5248-YGIJN")
# 
# tenure_stats <- calc_customer_tenure_stats(data$tenure, data$TotalCharges)
# 
# products <- renderProductList(data)
# 
# predict(xgb_mod,data[1,], type = "prob")
