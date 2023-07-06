library(shiny)
library(bs4Dash)
library(htmltools)
library(thematic)
library(reactable)
library(shinyWidgets)

# AVERAGE CAC TELCO $315

ui <- dashboardPage(
    title = "Admin Panel",
    fullscreen = TRUE,
    header = dashboardHeader(
        title = "Customer Dashboard",
        skin = "dark",
        status = "black",border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("screwdriver-wrench"),
        fixed = FALSE
    ),
    sidebar = dashboardSidebar(
        skin = "dark",
        status = "secondary",
        elevation = 3,
        sidebarMenu(
            sidebarHeader("Menu"),
            menuItem(
                "Existing Customers",
                tabName = "customers",
                icon = icon(name = "user")
            )
        )
    ),
    controlbar = dashboardControlbar(
        skin = "light",
        pinned = F,
        collapsed = F,
        overlay = FALSE
    ),
    footer = dashboardFooter(
        left = a(href = "https://github.com/dustinshook/customer-churn",
                 target = "_blank", "Dustin Shook"),
        right = "2023"
    ),
    body = dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
            tags$script(src = "js/main.js")
        ),
        
        # MODULES #
        customerUI("existing")
    )
)
