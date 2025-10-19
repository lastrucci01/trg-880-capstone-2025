library(shiny)
library(haven)
library(tidyr)
library(dplyr)
library(VGAM)
library(glmnet)
library(shinyjs)
library(reticulate)
options(scipen=999)


# Initialize Python environment
#use_virtualenv("~/.virtualenvs/r-reticulate", required = TRUE)
virtualenv_create("r-reticulate")

# Tell reticulate to use it
use_virtualenv("r-reticulate", required = TRUE)
joblib <- import("joblib")
np <- import("numpy")

df <- readRDS("clean_selected_collapsed_grouped_Ethan.rds")
model_multi  <- readRDS("multinomial_model.rds")
cv_fit <- model_multi$model
xnames <- model_multi$xnames
coef(cv_fit)

FDA_MARS <- readRDS("final_FDA_MARS_model.rds")

tf <- df %>% 
  mutate(TAR_STATUS = if_else(STATUS %in% c("CAN_LAP","SUR"), 1, 0))


# Load HistGradientBoosting model
histgrad_model <- joblib$load("histgradboost_model.pkl")
label_encoder <- joblib$load("label_encoder.pkl")

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
                               choices = c("Multinomial Logistic", "HistGradientBoosting", "FDA-MARS"),
                               selected = "Multinomial Logistic"),
                   
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
                            numericInput("INCOME", "Income:", value = 10000, min = 0),
                            selectInput("SECTOR", "Sector:",
                                        choices = levels(tf$SECTOR)),
                            selectInput("COMPANY_NAME", "Company Name:",
                                        choices = levels(tf$COMPANY_NAME))
                            
                            
                     ),
                     column(4,
                            numericInput("TERM", "Policy Term (Years):", value = 10, min = 1),
                            numericInput("ps_lapse1", "Broker Lapse Performance (%):",
                                         value = 95, min = 0, max = 100),
                            
                           
                            selectInput("Gender", "Payer Gender:", choices = c("MALE", "FEMALE")),
                            selectInput("PAYER_MARITAL_STATUS", "PAYER_MARITAL_STATUS:",
                                        choices = c("S", "M", "W") )
                          
                     ),
                     column(4,
                            selectInput("PRODUCT_GROUP", "Product Code:",
                                        choices = c("FUNERAL", "HEALTH", "INVEST", "RISK", "UNKNOWN")),
                            selectInput("AGE_AT_COMMENCEMENT", "Age at Commencement:",
                                        choices = c( "18-25", "26-35", "36-45", "46-55")),
                            selectInput("PAYMENT_MODE", "Payment Mode:",
                                        choices = c("ADD", "RSO", "DSO","ASO")),
                            selectInput("PAYPOINT_NAME", "Paypoint:",
                                        choices = levels(tf$PAYPOINT_NAME)),
                            
                            
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
  
  observe({
    
    
    multinomial_inputs <- c("PAYER_AGE_GROUP", "PRODUCT_GROUP", "PREMIUM",
                            "INCOME", "PAYMENT_MODE", "TERM",
                            "PAYER_GENDER", "PS_LAPSE1")
    
    histgb_inputs <- c("INCOME", "PS_LAPSE1", "SECTOR", "PAYMENT_MODE",
                       "PAYER_GENDER", "PRODUCT_GROUP", "PAYER_AGE_GROUP",
                       "PREMIUM", "TERM", "PAYER_MARITAL_STATUS",
                       "COMPANY_NAME", "PAYPOINT_NAME")
    
    fda_mars_inputs <- c("INCOME", "PREMIUM", "PS_LAPSE1", "SECTOR", "PAYMENT_MODE",
                         "PAYER_GENDER", "PRODUCT_GROUP", "PAYER_AGE_GROUP",
                         "TERM", "PAYER_MARITAL_STATUS", "COMPANY_NAME",
                         "PAYPOINT_NAME")
    
    # Combine all unique IDs across models
    all_inputs <- unique(c(multinomial_inputs, histgb_inputs, fda_mars_inputs))
    
    # Hide everything initially
    lapply(all_inputs, shinyjs::hide)
    
    # Show only those relevant for the selected model
    if (input$model_type == "Multinomial Logistic") {
      lapply(multinomial_inputs, shinyjs::show)
    } else if (input$model_type == "HistGradientBoosting") {
      lapply(histgb_inputs, shinyjs::show)
    } else if (input$model_type == "FDA-MARS") {
      lapply(fda_mars_inputs, shinyjs::show)
    }
  })
  
  

  model_dynamic <- reactive({
    data_used <- tf  # full or grouped data
    
    if (input$model_type == "Binary Logistic") {
      glm(
        TAR_STATUS ~ PREMIUM + INCOME + TERM + PAYER_AGE_GROUP + 
          PS_LAPSE1 + PAYER_GENDER + INSURED_GENDER + PRODUCT_GROUP,
        data = data_used,
        family = binomial(link = "logit")
      )
    } else if (input$model_type == "Multinomial Logistic") {
      model_multi
    }else if (input$model_type == "HistGradientBoosting") {
      histgrad_model  # Return the loaded model
    }else if(input$model_type == "FDA-MARS"){
      FDA_MARS
    }
    
  })
  

  output$model_summary <- renderPrint({
    summary(model_dynamic())
  })
  
  # --- Prediction Tab Logic ---
  observeEvent(input$predict_btn, {
    req(model_dynamic())
    model_type <- input$model_type
    if (model_type == "Multinomial Logistic") {
      new_data <- data.frame(
        PREMIUM = input$PREMIUM,
        INCOME = input$INCOME,
        TERM = input$TERM,
        PAYER_AGE_GROUP = factor(input$AGE_AT_COMMENCEMENT,
                                 levels = levels(tf$PAYER_AGE_GROUP)),
        PAYMENT_MODE = factor(input$PAYMENT_MODE,
                              levels = levels(tf$PAYMENT_MODE)),
        PRODUCT_GROUP = factor(input$PRODUCT_GROUP,
                               levels = levels(tf$PRODUCT_GROUP)),
        PAYER_GENDER = factor(input$Gender,
                              levels = levels(tf$PAYER_GENDER)),
        PS_LAPSE1 = input$ps_lapse1
      )
    } else if(input$model_type == "HistGradientBoosting") {
      new_data <- data.frame(
        INCOME = input$INCOME,
        PS_LAPSE1 = input$PS_LAPSE1,
        SECTOR = factor(input$SECTOR, levels = levels(tf$SECTOR)),
        PAYMENT_MODE = factor(input$PAYMENT_MODE,
                              levels = levels(tf$PAYMENT_MODE)),
        PAYER_GENDER = factor(input$PAYER_GENDER,
                              levels = levels(tf$PAYER_GENDER)),
        PRODUCT_GROUP = factor(input$PRODUCT_GROUP,
                               levels = levels(tf$PRODUCT_GROUP)),
        PAYER_AGE_GROUP = factor(input$PAYER_AGE_GROUP,
                                 levels = levels(tf$PAYER_AGE_GROUP)),
        PREMIUM = input$PREMIUM,
        TERM = input$TERM,
        PAYER_MARITAL_STATUS = factor(input$PAYER_MARITAL_STATUS,
                                      levels = levels(tf$PAYER_MARITAL_STATUS)),
        COMPANY_NAME = factor(input$COMPANY_NAME,
                              levels = levels(tf$COMPANY_NAME)),
        PAYPOINT_NAME = factor(input$PAYPOINT_NAME,
                               levels = levels(tf$PAYPOINT_NAME))
      )
      # HistGradientBoosting prediction with 12 features
      # Order: INCOME, PS_LAPSE1, SECTOR, PAYMENT_MODE, PAYER_GENDER, PRODUCT_GROUP,
      #        PAYER_AGE_GROUP, PREMIUM, TERM, PAYER_MARITAL_STATUS, COMPANY_NAME, PAYPOINT_NAME
     
    } else if(input$model_type == "FDA MARS"){
      new_data <- data.frame(
        INCOME = input$INCOME,
        PS_LAPSE1 = input$PS_LAPSE1,
        SECTOR = factor(input$SECTOR, levels = levels(tf$SECTOR)),
        PAYMENT_MODE = factor(input$PAYMENT_MODE,
                              levels = levels(tf$PAYMENT_MODE)),
        PAYER_GENDER = factor(input$PAYER_GENDER,
                              levels = levels(tf$PAYER_GENDER)),
        PRODUCT_GROUP = factor(input$PRODUCT_GROUP,
                               levels = levels(tf$PRODUCT_GROUP)),
        PAYER_AGE_GROUP = factor(input$PAYER_AGE_GROUP,
                                 levels = levels(tf$PAYER_AGE_GROUP)),
        PREMIUM = input$PREMIUM,
        TERM = input$TERM,
        PAYER_MARITAL_STATUS = factor(input$PAYER_MARITAL_STATUS,
                                      levels = levels(tf$PAYER_MARITAL_STATUS)),
        COMPANY_NAME = factor(input$COMPANY_NAME,
                              levels = levels(tf$COMPANY_NAME)),
        PAYPOINT_NAME = factor(input$PAYPOINT_NAME,
                               levels = levels(tf$PAYPOINT_NAME))
      )
    }
    
    
    if (input$model_type == "Binary Logistic") {
      predicted_prob <- predict(model_dynamic(), newdata = new_data, type = "response")
      output_text <- paste0("Predicted Probability of Lapse: ", round(predicted_prob * 100, 2), "%")
      
    } else if (input$model_type == "Multinomial Logistic") {
      
      train_colnames <- xnames
      newx_raw <- as.matrix(model.matrix(
        ~  PAYER_AGE_GROUP + PRODUCT_GROUP + PREMIUM + INCOME +
          PAYMENT_MODE + TERM + PAYER_GENDER  + PS_LAPSE1,
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
    } else if(input$model_type == "HistGradientBoosting"){
      feature_vector <- c(
        new_data$INCOME,
        new_data$PS_LAPSE1,
        1, # SECTOR placeholder
        as.numeric(new_data$PAYMENT_MODE),
        as.numeric(new_data$PAYER_GENDER),
        as.numeric(new_data$PRODUCT_GROUP),
        as.numeric(new_data$PAYER_AGE_GROUP),
        new_data$PREMIUM,
        new_data$TERM,
        as.numeric(new_data$PAYER_MARITAL_STATUS),
        1, # COMPANY_NAME placeholder
        1  # PAYPOINT_NAME placeholder
      )
      
      # Convert to matrix for prediction
      X_single <- r_to_py(matrix(feature_vector, nrow = 1))
      
      # Make prediction
      pred <- histgrad_model$predict(X_single)
      prob <- histgrad_model$predict_proba(X_single)
      
      # Get results
      pred_int <- as.integer(py_to_r(pred))
      pred_label <- py_to_r(label_encoder$inverse_transform(r_to_py(array(pred_int))))
      probabilities <- prob
      classes <- py_to_r(label_encoder$classes_)
      
      output_text <- paste0(
        "Predicted Policy Status Probabilities:\n",
        paste0(classes, ": ", probabilities, collapse = "\n")
      )
    } else if(input$model_type == "FDA MARS"){
      posterior_probs_new_observation <- predict(final_FDA_MARS_model, newdata = new_data , type = "posterior" )
      output_text <- paste0(
        "Predicted Policy Status Probabilities:\n",
        paste0(classes, ": ", posterior_probs_new_observation, collapse = "\n")
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