library(shiny)
library(dplyr)
library(readr)
library(mice)
library(ggplot2)

make_methods <- function(data, numeric_method) {
  m <- make.method(data)
  m[] <- ""
  m["Salary"] <- numeric_method
  m["Years_Exp"] <- numeric_method
  m["Job_Satisfaction"] <- numeric_method
  m["TimeSearching"] <- "polyreg"
  m["Uses_AI"] <- "logreg"
  m["Is_Remote"] <- "logreg"
  m
}

normalize_types <- function(df) {
  df %>%
    mutate(
      Salary = as.numeric(Salary),
      Years_Exp = as.numeric(Years_Exp),
      Job_Satisfaction = as.numeric(Job_Satisfaction),
      Uses_AI = factor(Uses_AI, levels = c(0, 1)),
      Is_Remote = factor(Is_Remote, levels = c(0, 1)),
      TimeSearching = factor(TimeSearching)
    )
}

calc_metrics <- function(train, test, formula) {
  fit <- lm(formula, data = train)
  pred <- predict(fit, newdata = test)
  y <- model.response(model.frame(formula, data = test))

  sse <- sum((y - pred) ^ 2, na.rm = TRUE)
  sst <- sum((y - mean(y, na.rm = TRUE)) ^ 2, na.rm = TRUE)
  test_r2 <- if (sst == 0) NA_real_ else 1 - sse / sst
  rmse <- sqrt(mean((y - pred) ^ 2, na.rm = TRUE))

  list(
    fit = fit,
    pred = pred,
    y = y,
    adj_r2 = summary(fit)$adj.r.squared,
    test_r2 = test_r2,
    rmse = rmse
  )
}

ui <- fluidPage(
  titlePanel("Imputation + Model Tuning Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("data_path", "Data path", "outputs/data_filtered_columns.csv"),
      selectInput(
        "num_method",
        "Numeric imputation method",
        choices = c("cart", "pmm", "norm", "rf"),
        selected = "cart"
      ),
      sliderInput("m", "Imputations (m)", min = 1, max = 5, value = 3),
      sliderInput("maxit", "Max iterations", min = 2, max = 10, value = 5),
      numericInput("seed", "Seed", value = 42, min = 1, max = 1000000, step = 1),
      sliderInput("train_prop", "Train split", min = 0.6, max = 0.9, value = 0.8),
      checkboxInput("use_log", "Use log1p(Salary)", value = TRUE),
      textInput(
        "predictors",
        "Predictors",
        "Uses_AI + Years_Exp + Is_Remote + TimeSearching + Job_Satisfaction"
      ),
      checkboxInput("avg_m", "Average metrics across m imputations", value = TRUE),
      actionButton("run", "Run"),
      helpText("Note: imputation runs before train/test split for speed; results may be optimistic.")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tableOutput("metrics"),
      plotOutput("scatter"),
      tableOutput("coef")
    )
  )
)

server <- function(input, output, session) {
  results <- eventReactive(input$run, {
    if (!file.exists(input$data_path)) {
      return(list(error = paste("File not found:", input$data_path)))
    }

    raw_df <- read_csv(input$data_path, show_col_types = FALSE)
    base_df <- normalize_types(raw_df)

    imp_df <- base_df %>%
      select(Salary, Uses_AI, Years_Exp, Job_Satisfaction, Is_Remote, TimeSearching)

    response <- if (isTRUE(input$use_log)) "log1p(Salary)" else "Salary"
    formula_text <- paste(response, "~", input$predictors)
    model_formula <- tryCatch(
      as.formula(formula_text),
      error = function(e) NA
    )

    if (length(model_formula) == 1 && is.na(model_formula)) {
      return(list(error = "Invalid formula. Please check predictors."))
    }

    imp <- mice(
      imp_df,
      m = input$m,
      method = make_methods(imp_df, input$num_method),
      seed = input$seed,
      maxit = input$maxit,
      printFlag = FALSE
    )

    run_one <- function(completed) {
      df <- normalize_types(completed)
      set.seed(input$seed)
      n <- nrow(df)
      idx <- sample.int(n, size = floor(input$train_prop * n))
      train <- df[idx, , drop = FALSE]
      test <- df[-idx, , drop = FALSE]
      calc_metrics(train, test, model_formula)
    }

    if (isTRUE(input$avg_m) && imp$m > 1) {
      runs <- lapply(1:imp$m, function(i) run_one(complete(imp, i)))
      metrics <- tibble(
        adj_r2 = mean(sapply(runs, function(x) x$adj_r2), na.rm = TRUE),
        test_r2 = mean(sapply(runs, function(x) x$test_r2), na.rm = TRUE),
        rmse = mean(sapply(runs, function(x) x$rmse), na.rm = TRUE)
      )
      plot_run <- runs[[1]]
      list(
        error = NULL,
        metrics = metrics,
        plot = tibble(actual = plot_run$y, predicted = plot_run$pred),
        coef = summary(plot_run$fit)$coefficients,
        formula = formula_text
      )
    } else {
      run <- run_one(complete(imp, 1))
      list(
        error = NULL,
        metrics = tibble(
          adj_r2 = run$adj_r2,
          test_r2 = run$test_r2,
          rmse = run$rmse
        ),
        plot = tibble(actual = run$y, predicted = run$pred),
        coef = summary(run$fit)$coefficients,
        formula = formula_text
      )
    }
  })

  output$status <- renderText({
    res <- results()
    if (!is.null(res$error)) {
      return(res$error)
    }
    paste("Model:", res$formula)
  })

  output$metrics <- renderTable({
    res <- results()
    if (!is.null(res$error)) {
      return(NULL)
    }
    res$metrics
  }, digits = 4)

  output$scatter <- renderPlot({
    res <- results()
    if (!is.null(res$error)) {
      return(NULL)
    }
    ggplot(res$plot, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Actual", y = "Predicted", title = "Predicted vs Actual") +
      theme_minimal()
  })

  output$coef <- renderTable({
    res <- results()
    if (!is.null(res$error)) {
      return(NULL)
    }
    coef_df <- as.data.frame(res$coef)
    coef_df$term <- rownames(coef_df)
    coef_df[, c("term", colnames(res$coef))]
  }, digits = 4)
}

shinyApp(ui, server)
