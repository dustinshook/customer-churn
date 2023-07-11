customerUI <- function(id) {
  ns <- NS(id)
  tabItem(
      tabName = "customers",
      box(
          title = "Customer Search", 
          width = 4,
          uiOutput(ns("customer_picker"))
      ),
      
      box(
          title = "Products",
          width = 4,
          reactableOutput(ns("cust_products_tbl"))
      ),
      
      box(
          title = "Payments",
          width = 8,
          reactableOutput(ns("cust_payments_tbl"))
      )
  )
}

customerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
        
      customerData <- reactive({
          get_telco_data(FALSE)
      })
      
      output$customer_picker <- renderUI({
          ns <- session$ns
          
          pickerInput(
              inputId = ns("cust_search"),
              label = "Customer ID", 
              choices = pull(customerData(), customerID),
              options = list(
                  `live-search` = TRUE)
          )
      })
      
      pickedCustomer <- reactive({
          req(customerData())
          req(input$cust_search)
          
          customerData() %>% 
              filter(customerID == input$cust_search)
      })
      
      output$cust_products_tbl <-
          renderReactable({
              reactable(
                  renderProductList(
                      pickedCustomer()
                  ),
                  
                  columns = list(
                      name = colDef(
                          name = "PRODUCT"
                      ),
                      
                      value = colDef(
                          name = "STATUS"
                      )
                  )
              )
          })
      
      output$cust_payments_tbl <-
          renderReactable({
              reactable(
                  calc_customer_tenure_stats(
                      pickedCustomer()$tenure,
                      pickedCustomer()$TotalCharges
                  ),
                  selection = "multiple",
                  defaultPageSize = 10,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  
                  columns = list(
                      amount = colDef(
                          name = "AMOUNT",
                          format = colFormat(prefix = "$", separators = TRUE, digits = 2),
                          html = TRUE,
                          cell = JS("paymentBadge")
                      ),
                      month = colDef(
                          name = "DATE",
                          format = colFormat(date = TRUE)
                      ),
                      invoice_id = colDef(
                          name = "DESCRIPTION"
                      ),
                      charges = colDef(
                          name = "TOTAL SPEND",
                          format = colFormat(prefix = "$", separators = TRUE, digits = 2)
                      )
                  ),
                  
                  defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom")
              )
          })
    }
  )
}