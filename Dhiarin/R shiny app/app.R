library(shiny)
library(haven)
library(tidyr)
library(dplyr)
library(VGAM)
library(glmnet)
library(shinyjs)
options(scipen=999)

df <- readRDS("clean_selected_collapsed_grouped_Ethan.rds")
model_multi  <- readRDS("multinomial_model.rds")
cv_fit <- model_multi$model
xnames <- model_multi$xnames


tf <- df %>% 
  mutate(TAR_STATUS = if_else(STATUS %in% c("CAN_LAP","SUR"), 1, 0))


# ---- UI ----
ui <- navbarPage(
  title = "Client Retention Scoring App",
  
  # --- Tab 1: Model Prediction ---
  tabPanel("Model Prediction",
           fluidPage(
             useShinyjs(),
             
             tags$head(
               tags$style(HTML("
                  html, body {
                    background-color: #e8e8e8 !important;
                    height: 100%;
                  }
                  .left-container, .right-container, .output-container {
                    background-color: #ffffff;        
                    border-radius: 10px;              
                    padding: 20px;
                    margin-bottom: 15px;
                    box-shadow: -5px 5px 10px rgba(0,0,0,0.5);
                  }
                  
                  
                  .output-container {
                    background-color: #ffffff;
                  }
                  
                  
                  .btn-lg {
                    width: 100%;
                    font-size: 16px;
                    padding: 10px;
                  }
                  "))
             ),
             titlePanel("Client Retention Predictor"),
             
             # --- Main layout ---
             fluidRow(
               
               # ===== LEFT CONTAINER =====
               column(
                 width = 4,  # Controls width of left side (out of 12)
                 div(
                   class = "left-container",
                   h4("Model & Dataset Selection", style = "font-weight: bold"),
                   
                   # Model selection
                   selectInput("model_type", "Select Model Type:",
                               choices = c("Binary Logistic", "Multinomial Logistic"),
                               selected = "Binary Logistic"),
                   
                   # Dataset selection
                   selectInput("data_scope", "Select Dataset to Model On:",
                               choices = c("Full Dataset", "Grouped Dataset"),
                               selected = "Full Dataset"),
                   selectInput("group_var", "Select Grouping Variable:",
                               choices = c("PRODUCT_GROUP", "PAYMENT_MODE")),
                   uiOutput("group_value_ui"),
                   # Predict button
                   div(style = "margin-top: 10px;",
                       actionButton("predict_btn", "Predict Lapse Probability", class = "btn-primary"))
                 )
               ),
               
               # ===== RIGHT CONTAINER =====
               column(
                 width = 8,  # Right side: Inputs aligned horizontally
                 div(
                   class = "right-container",
                   h4("Enter Input Values", style = "font-weight: bold"),
                   fluidRow(
                     column(4,
                            numericInput("PREMIUM", "Premium:", value = 500, min = 0),
                            numericInput("INCOME", "Income:", value = 10000, min = 0)
                     ),
                     column(4,
                            numericInput("TERM", "Policy Term (Years):", value = 10, min = 1),
                            numericInput("ps_lapse1", "Broker Lapse Performance (%):",
                                         value = 95, min = 0, max = 100),
                            selectInput("PRODUCT_GROUP", "Product Code:",
                                        choices = c("FUNERAL", "HEALTH", "INVEST", "RISK", "UNKNOWN")),
                            selectInput("PAYMENT_MODE", "Payment Mode:",
                                        choices = c("ADD", "RSO", "DSO"))
                     ),
                     column(4,
                            selectInput("AGE_AT_COMMENCEMENT", "Age at Commencement:",
                                        choices = c( "18-25", "26-35", "36-45", "46-55")),
                            selectInput("Gender", "Payer Gender:", choices = c("MALE", "FEMALE")),
                            selectInput("Insured_Gender", "Insured Gender:", choices = c("MALE", "FEMALE")),
                            selectInput("PAYER_MARITAL_STATUS", "PAYER_MARITAL_STATUS:",
                                        choices = c("S", "M", "W")),
                            selectInput("INSURED_MARITAL_STATUS", "INSURED_MARITAL_STATUS:",
                                        choices = c("S", "M", "W"))
                     )
                   ),
                   
                   
                 )
               )
             ),
             
             # ===== OUTPUT CONTAINER (below both columns) =====
             fluidRow(
               column(
                 width = 12,
                 div(
                   class = "output-container",
                   h4("Prediction Result", style = "font-weight: bold"),
                   verbatimTextOutput("prediction_output")
                 )
               )
             )
           )
  ),
  
  # --- Tab 2: Histogram Explorer ---
  tabPanel("Histogram Explorer",
           fluidPage(
             titlePanel("Histogram Explorer"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_select",
                             "Select a numeric variable:",
                             choices = c("PREMIUM", "INCOME", "TERM")),
                 sliderInput("bins",
                             "Number of bins:",
                             min = 5, max = 100, value = 30)
               ),
               mainPanel(
                 h4("Frequency Histogram"),
                 plotOutput("hist_plot")
               )
             )
           )
  ),
  
  # --- Tab 3: About ---
  tabPanel("Scatter Plot Explorer",
           fluidPage(
             titlePanel("Scatter Plot Explorer"),
             
             sidebarLayout(
               sidebarPanel(
                 # Select X and Y variables
                 selectInput("x_var",
                             "Select X-axis variable:",
                             choices = c("PREMIUM", "INCOME", "TERM",
                                         "PAYFREQ", "AGE_AT_COMMENCEMENT",
                                         "nbanks", "nloans", "ps_lapse1"),
                             selected = "INCOME"),
                 
                 selectInput("y_var",
                             "Select Y-axis variable:",
                             choices = c("PREMIUM", "INCOME", "TERM",
                                         "PAYFREQ", "AGE_AT_COMMENCEMENT",
                                         "nbanks", "nloans", "ps_lapse1"),
                             selected = "PREMIUM"),
                 
                 checkboxInput("show_trend", "Add regression line", value = TRUE)
               ),
               
               mainPanel(
                 h4("Scatter Plot"),
                 plotOutput("scatter_plot")
               )
             )
           )
  )
)


# ---- SERVER ----
server <- function(input, output, session) {
  
  
  output$group_value_ui <- renderUI({
    req(input$group_var)
    vals <- unique(tf[[input$group_var]])
    selectInput("group_value", paste("Select", input$group_var, "value:"), choices = vals)
  })
  
  
  df_selected <- reactive({
    if (input$data_scope == "Full Dataset") {
      tf
    } else {
      req(input$group_var, input$group_value)
      subset(tf, tf[[input$group_var]] == input$group_value)
    }
  })
  
  observe({
    if (input$data_scope == "Full Dataset") {
      if (!is.null(input$group_var)) shinyjs::disable("group_var")
      if (!is.null(input$group_value)) shinyjs::disable("group_value")
    } else {
      shinyjs::enable("group_var")
      shinyjs::enable("group_value")
    }
  })
  
  

  model_dynamic <- reactive({
    data_used <- df_selected()  # full or grouped data
    
    if (input$model_type == "Binary Logistic") {
      glm(
        TAR_STATUS ~ PREMIUM + INCOME + TERM + PAYER_AGE_GROUP + 
          PS_LAPSE1 + PAYER_GENDER + INSURED_GENDER + PRODUCT_GROUP,
        data = data_used,
        family = binomial(link = "logit")
      )
    } else if (input$model_type == "Multinomial Logistic") {
      model_multi
    }
    
  })
  

  output$model_summary <- renderPrint({
    summary(model_dynamic())
  })
  
  # --- Prediction Tab Logic ---
  observeEvent(input$predict_btn, {
    req(model_dynamic())
    
    new_data <- data.frame(
      PREMIUM = input$PREMIUM,
      INCOME = input$INCOME,
      TERM = input$TERM,
      PAYER_AGE_GROUP = factor(input$AGE_AT_COMMENCEMENT , levels = c( 
                                                                          "18-25",
                                                                          "26-35",
                                                                          "36-45",
                                                                          "46-55")),
      PAYER_MARITAL_STATUS = factor(input$PAYER_MARITAL_STATUS, levels = c("S", "M", "W")),
      INSURED_MARITAL_STATUS = factor(input$INSURED_MARITAL_STATUS, levels = c("S", "M", "W")),
      PAYMENT_MODE = factor(input$PAYMENT_MODE, levels = c("ADD", "RSO", "DSO")),
      PS_LAPSE1 = input$ps_lapse1,
      PAYER_GENDER = factor(input$Gender, levels = c("FEMALE", "MALE")),
      INSURED_GENDER = factor(input$Insured_Gender, levels = c("FEMALE", "MALE")),
      PRODUCT_GROUP = factor(input$PRODUCT_GROUP, levels = c("FUNERAL", "HEALTH", "INVEST", "RISK", "UNKNOWN"))
    )
    
    
    if (input$model_type == "Binary Logistic") {
      predicted_prob <- predict(model_dynamic(), newdata = new_data, type = "response")
      output_text <- paste0("Predicted Probability of Lapse: ", round(predicted_prob * 100, 2), "%")
      
    } else if (input$model_type == "Multinomial Logistic") {
      x <- model.matrix(
        STATUS ~ PAYER_AGE_GROUP + PRODUCT_GROUP + PREMIUM + INCOME +
          PAYMENT_MODE + TERM + PAYER_GENDER + PAYER_MARITAL_STATUS +
          INSURED_MARITAL_STATUS + INSURED_GENDER + PS_LAPSE1,
        data = tf
      )[, -1] 
      
      train_colnames <- xnames
      newx_raw <- as.matrix(model.matrix(
        ~ PAYER_AGE_GROUP + PRODUCT_GROUP + PREMIUM + INCOME +
          PAYMENT_MODE + TERM + PAYER_GENDER + PAYER_MARITAL_STATUS +
          INSURED_MARITAL_STATUS + INSURED_GENDER + PS_LAPSE1,
        data = new_data
      )[, -1, drop = FALSE])
      
      newx_aligned <- matrix(0, nrow = 1, ncol = length(train_colnames))
      colnames(newx_aligned) <- train_colnames
      
      # Copy overlapping columns
      common_cols <- intersect(colnames(newx_raw), train_colnames)
      newx_aligned[, common_cols] <- newx_raw[, common_cols, drop = FALSE]
      
      # Predict safely
      predicted_probs_glmnet <- predict(cv_fit, newx = newx_aligned,
                                        s = "lambda.min", type = "response")
      
      prob_df <- as.data.frame(predicted_probs_glmnet[,,1])
      
      output_text <- paste0(
        "Predicted Policy Status Probabilities:\n",
        paste0(rownames(prob_df), ": ", prob_df[, 1], collapse = "\n")
      )
    }
    
    output$prediction_output <- renderText({
      paste(output_text, "\n(Model type:", input$model_type, ")")
    })
  })
  
  
  
  

  
  
  # --- Histogram Tab Logic ---
  output$hist_plot <- renderPlot({
    req(input$var_select)
    x <- tf[[input$var_select]]
    x <- x[!is.na(x)]
    
    hist(
      x,
      breaks = input$bins,
      col = "skyblue",
      border = "white",
      main = paste("Histogram of", input$var_select),
      xlab = input$var_select,
      ylab = "Frequency"
    )
  })
  
  
  # --- Scatter Plot Tab Logic ---
  output$scatter_plot <- renderPlot({
    req(input$x_var, input$y_var)
    
    x <- tf[[input$x_var]]
    y <- tf[[input$y_var]]
    
    # Remove missing values
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
    
    # Base scatter plot
    plot(
      x, y,
      xlab = input$x_var,
      ylab = input$y_var,
      main = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
      col = "steelblue",
      pch = 19
    )
    
    # Optional regression line
    if (input$show_trend) {
      abline(lm(y ~ x), col = "red", lwd = 2)
    }
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)