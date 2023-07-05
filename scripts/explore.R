library(tidyverse)
library(GGally)
library(skimr)
library(tidyquant)

raw_data <- 
    readr::read_csv('data/WA_Fn-UseC_-Telco-Customer-Churn.csv')

skimr::skim(raw_data)

# Character Data proportion 
prop_table <- raw_data %>% 
    select(-customerID) %>% 
    select(where(is.character)) %>% 
    map(~table(.) %>% prop.table())

raw_data %>% 
    count(Churn, StreamingTV)


raw_data %>% 
    select(Churn, where(is.numeric) ) %>% 
    ggpairs(aes(color = Churn), lower = 'blank', legend = 1, diag = list(continuous = wrap('densityDiag', alpha = .5))) +
    theme(legend.position = "bottom")


plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = tidyquant::palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
    
    data_factored <- data %>%
        mutate(across(where(is.character), ~ as_factor(.x) %>% as.numeric())) %>% 
        pivot_longer(everything(), names_to = 'key') %>% 
        arrange(key)
    
    if (fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        tidyquant::theme_tq()
    
    return(g)
    
}

raw_data %>% 
    select(-customerID) %>% 
    plot_hist_facet(bins = 10, ncol = 5)


skewed_feature_names <- raw_data %>% 
    select(where(is.numeric)) %>% 
    map_df(skewness) %>% 
    pivot_longer(everything()) %>% 
    arrange(desc(value)) %>% 
    filter(value >= 0.8) %>% 
    pull(name) %>% 
    as.character()

raw_data %>%
    select(
        Churn, where(is.character)
    ) %>%
    select(-c(customerID, gender)) %>% 
    pivot_longer(!Churn) %>%
    ggplot(aes(y = value, fill = Churn)) +
    geom_bar(position = "fill") +
    facet_wrap(vars(name), scales = "free", ncol = 2) +
    labs(x = NULL, y = NULL, fill = NULL)
