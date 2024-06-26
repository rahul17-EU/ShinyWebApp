## Red Wine Quality - ML models
#0. Load libraries, Load Data ------------------------------------------------------------------------------
# List of packages
packages <- c("tidyverse", "tidymodels", "doParallel", "parsnip", "ranger", "rpart.plot", "DiagrammeR", "xgboost")

# Loop over the list of packages
for(pkg in packages){
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}

wine  <- read.csv("G:/My Drive/Studies/Term 2/R/Final Project/DATA/winequality-red.csv")

#1. Train Test Split and Cross Folds ------------------------------------------------------------------------------
# Split the data into a training set and a test set
set.seed(123) # for reproducibility
wine_split <- initial_split(wine, prop = 0.7, strata = quality)
train <- training(wine_split)
test <- testing(wine_split)

# create 10 fold cross validation object also using stratified sampling
wine_folds <- vfold_cv(data = wine,
                  v = 10,
                  strata = quality)


#2. Recipe ------------------------------------------------------------------------------
# recipe. All variables numeric, but not scaled or centered
wine_recipe <- recipe(quality ~ ., data = train) %>%
    step_scale(all_predictors()) %>%
    step_center(all_predictors())


#3. Metrics ------------------------------------------------------------------------------
metrics <- metric_set(mae, rmse, rsq)


#4. Model 1: Linear Regression ------------------------------------------------------------------------------
# Linear Regression Specifications
lm_spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

# No tuning parameters for basic linear regression

# Register a parallel backend using doParallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Combine the recipe and model into a workflow. We use our basic recipe wine_recipe
lm_wf <- workflow() %>%
    add_recipe(wine_recipe) %>%
    add_model(lm_spec)

# Fit the model using the resamples
lm_results <- fit_resamples(
    lm_wf,
    resamples = wine_folds,
    metrics = metrics,
    control = control_resamples(verbose = TRUE)
)

# Stop the cluster
stopCluster(cl)

# Collect and interpret the metrics
lm_metrics <- collect_metrics(lm_results)

lm_metrics_summary <- lm_metrics %>%
    group_by(.metric) %>%
    summarize(
        mean = mean(mean, na.rm = TRUE),
        std = mean(std_err, na.rm = TRUE)
    )

# Print the summary
print(lm_metrics_summary)

# # A tibble: 3 × 3
#   .metric  mean     std
#   <chr>   <dbl>   <dbl>
# 1 mae     0.504 0.00954
# 2 rmse    0.650 0.0117
# 3 rsq     0.355 0.00928

# Fit the model on the training data
lm_fit <- fit(lm_wf, data = train)

# Extract the fitted model from the workflow
lm_fit_model <- pull_workflow_fit(lm_fit)

# Extract the formula
lm_formula <- formula(lm_fit_model$fit)

# Extract the coefficients
lm_coefs <- coef(lm_fit_model$fit)

# Construct the formula
lm_formula_with_coefs <- paste0("y = ", round(lm_coefs[1], 2), 
                                paste(" + ", names(lm_coefs[-1]), " * ", round(lm_coefs[-1], 2), collapse = ""))

# Print the formula
print(lm_formula_with_coefs)

# Save the fitted model to a file
saveRDS(lm_fit_model, "lm_fit_model.rds")

# Load the fitted model from the file
lm_fit_model <- readRDS("lm_fit_model.rds")


#5. Model 2: Decision Tree ------------------------------------------------------------------------------

# Decision Tree Specifications
tree_spec <- decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune()
) %>%
    set_mode("regression") %>%
    set_engine("rpart")

# Set up tune grid for decision tree using cost complexity and tree depth as hyperparameters
tree_grid <- grid_regular(
    cost_complexity(range = c(0.001, 0.1), trans = scales::log10_trans()),
    tree_depth(range = c(1, 100)),
    levels = 20
)

# Combine the recipe and model into a workflow
tree_wf <- workflow() %>%
    add_recipe(wine_recipe) %>%
    add_model(tree_spec)

# Register a parallel backend using doParallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run the grid search
tree_results <- tune_grid(
    tree_wf,
    resamples = wine_folds,
    grid = tree_grid,
    metrics = metrics,
    control = control_grid(verbose = TRUE)
)

# Stop the cluster
stopCluster(cl)

# Collect and interpret the metrics
tree_metrics <- collect_metrics(tree_results)

# Summarize the decision tree metrics
tree_metrics_summary <- tree_metrics %>%
    group_by(.metric) %>%
    summarize(
        mean = mean(mean, na.rm = TRUE),
        std = mean(std_err, na.rm = TRUE)
    )

# Print the summary
print(tree_metrics_summary)

# # A tibble: 3 × 3
#   .metric    mean       std
#   <chr>     <dbl>     <dbl>
# 1 mae       0.683   0.00646
# 2 rmse      0.807   0.0114
# 3 rsq     NaN     NaN

# This tree seems to perform horribly.

# Find the best hyperparameters
best_tree_params <- tree_results %>%
    select_best(metric = "rmse")

# Print the best hyperparameters
print(best_tree_params)

# The best tree found via the grid search has a tree depth of 1. This is a very shallow tree.
# This explains the poor performance of the decision tree model.
# Still, we will visualize the tree to show clearly what it does and why it performs so poorly.

# Finalize the workflow with the best hyperparameters
final_tree_wf <- finalize_workflow(tree_wf, best_tree_params)

# Fit the model on the training data
tree_fit_model <- fit(final_tree_wf, data = train)

# Save the fitted model to a file
saveRDS(tree_fit_model, "final_tree_fit.rds")

# Load the fitted model from the file
tree_fit_model <- readRDS("final_tree_fit.rds")

# Extract the model from the workflow
tree_model <- pull_workflow_fit(tree_fit_model)$fit

# Visualize the tree
rpart.plot::rpart.plot(tree_model)

# Save the "tree"
# Create a PDF file
pdf("decision_tree_plot.pdf")

# Plot the tree
rpart.plot::rpart.plot(tree_model)

# Close the PDF file
dev.off()

# The tree simply predicts the average quality of the training set for all observations.


#6. Model 3: Random Forest ------------------------------------------------------------------------------
# Random Forest Specifications
rf_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
) %>%
    set_mode("regression") %>%
    set_engine("ranger", importance = 'impurity')

# Set up tune grid for random forest using mtry and min_n as hyperparameters
rf_grid <- grid_regular(
    mtry(range = c(1, ncol(train))),
    min_n(range = c(1, 10)),
    levels = 5
)

# Combine the recipe and model into a workflow
rf_wf <- workflow() %>%
    add_recipe(wine_recipe) %>%
    add_model(rf_spec)

# Register a parallel backend using doParallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run the grid search
rf_results <- tune_grid(
    rf_wf,
    resamples = wine_folds,
    grid = rf_grid,
    metrics = metrics,
    control = control_grid(verbose = TRUE)
)

# Stop the cluster
stopCluster(cl)

# Collect and interpret the metrics
rf_metrics <- collect_metrics(rf_results)

# Summarize the random forest metrics
rf_metrics_summary <- rf_metrics %>%
    group_by(.metric) %>%
    summarize(
        mean = mean(mean, na.rm = TRUE),
        std = mean(std_err, na.rm = TRUE)
    )

# Print the summary
print(rf_metrics_summary)

# # A tibble: 3 × 3
#   .metric  mean    std
#   <chr>   <dbl>  <dbl>
# 1 mae     0.418 0.0105
# 2 rmse    0.573 0.0125
# 3 rsq     0.507 0.0147

# The random forest model performs better than the linear regression model.

# Find the best hyperparameters
best_rf_params <- rf_results %>%
    select_best(metric = "rmse")

# Print the best hyperparameters
print(best_rf_params)

# Finalize the workflow with the best hyperparameters
final_rf_wf <- finalize_workflow(rf_wf, best_rf_params)

# Fit the model on the training data
rf_fit_model <- fit(final_rf_wf, data = train)

# Save the fitted model to a file
saveRDS(rf_fit_model, "final_rf_fit.rds")

# Load the fitted model from the file
rf_fit_model <- readRDS("final_rf_fit.rds")

# Feature Importance Graph
# Extract the model from the workflow
rf_model <- extract_fit_parsnip(rf_fit_model)$fit

# Compute feature importance
importance <- importance(rf_model)

# Create a data frame for plotting
importance_df <- data.frame(
  Feature = c("Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides", 
              "Free Sulfur Dioxide", "Total Sulfur Dioxide", "Density", "pH", "Sulphates", "Alcohol"),
  Importance = importance
)

# Load the ggplot2 library
library(ggplot2)

# Create a feature importance plot
p <- ggplot(importance_df, aes(x = reorder(Feature, -Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Feature", y = "Importance", title = "Feature Importance")

# Save the plot to a file with custom dimensions
ggsave("rf_explainability.png", plot = p, width = 14, height = 6)




# Generate predictions on the test set
rf_test_predictions <- predict(rf_fit_model, new_data = test)

# Create a copy of the test set
test_rf_generalization_eval <- test

# Bind the predictions to the copy of the test set
test_rf_generalization_eval <- bind_cols(test_rf_generalization_eval, rf_test_predictions)

# Calculate performance metrics
rf_test_results <- metrics(test_rf_generalization_eval, truth = quality, estimate = .pred)

# # A tibble: 3 × 3
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 mae     standard       0.421
# 2 rmse    standard       0.578
# 3 rsq     standard       0.499

# Values close to the training set metrics indicate that the model generalizes well and 
# is not overfitting the training data.


#7. Model 4: XGBoost ------------------------------------------------------------------------------
# XGBoost Specifications
xgb_spec <- boost_tree(
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune()
) %>%
    set_mode("regression") %>%
    set_engine("xgboost")

# Set up tune grid for XGBoost
xgb_grid <- grid_regular(
    min_n(range = c(1, 10)),
    tree_depth(range = c(1, 20)),
    learn_rate(range = c(0.01, 0.1)),
    levels = 4
)

# Combine the recipe and model into a workflow
xgb_wf <- workflow() %>%
    add_recipe(wine_recipe) %>%
    add_model(xgb_spec)

# Register a parallel backend using doParallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Run the grid search
xgb_results <- tune_grid(
    xgb_wf,
    resamples = wine_folds,
    grid = xgb_grid,
    metrics = metrics,
    control = control_grid(verbose = TRUE)
)

# Stop the cluster
stopCluster(cl)

# Collect and interpret the metrics
xgb_metrics <- collect_metrics(xgb_results)

# Summarize the XGBoost metrics
xgb_metrics_summary <- xgb_metrics %>%
    group_by(.metric) %>%
    summarize(
        mean = mean(mean, na.rm = TRUE),
        std = mean(std_err, na.rm = TRUE)
    )

# Print the summary
print(xgb_metrics_summary)

# # A tibble: 3 × 3
#   .metric  mean    std
#   <chr>   <dbl>  <dbl>
# 1 mae     0.507 0.0143
# 2 rmse    0.746 0.0179
# 3 rsq     0.318 0.0212

# Performs worse than the random forest model.

# Find the best hyperparameters
best_xgb_params <- xgb_results %>%
    select_best(metric = "rmse")

# Print the best hyperparameters
print(best_xgb_params)

# Also only a tree depth of 1. This is a very shallow tree.

# Finalize the workflow with the best hyperparameters
final_xgb_wf <- finalize_workflow(xgb_wf, best_xgb_params)

# Fit the model on the training data
xgb_fit_model <- fit(final_xgb_wf, data = train)

# Plot the tree
# Extract the xgb.Booster object
xgb_model_fit <- xgb_fit_model %>% pull_workflow_fit()
xgb_booster <- xgb_model_fit$fit

# Create the plot and save it to a variable
xgboost::xgb.plot.tree(
    feature_names = colnames(train),
    model = xgb_booster
)

# The xgboost is a single line since we have a tree depth of 1.

# Save the fitted model to a file
saveRDS(xgb_fit_model, "final_xgb_fit.rds")

# Load the fitted model from the file
xgb_fit_model <- readRDS("final_xgb_fit.rds")

# Feature Importance Graph
# Extract the model from the workflow
xgb_model <- extract_fit_parsnip(xgb_fit_model)$fit

# Compute feature importance
importance <- xgb.importance(model = xgb_model)

# Create a data frame for plotting
importance_df <- data.frame(
    Feature = c("Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides", 
                            "Free Sulfur Dioxide", "Total Sulfur Dioxide", "Density", "pH", "Sulphates", "Alcohol"),
    Importance = importance$Gain
)

# Create a feature importance plot
p <- ggplot(importance_df, aes(x = reorder(Feature, -Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(x = "Feature", y = "Importance", title = "Feature Importance")


# Save the plot to a file with custom dimensions
ggsave("xgb_explainability.png", plot = p, width = 14, height = 6)



# Generate predictions on the test set
xgb_test_predictions <- predict(xgb_fit_model, new_data = test)

# Create a copy of the test set
test_xgb_generalization_eval <- test

# Bind the predictions to the copy of the test set
test_xgb_generalization_eval <- bind_cols(test_xgb_generalization_eval, xgb_test_predictions)

# Calculate performance metrics
xgb_test_results <- metrics(test_xgb_generalization_eval, truth = quality, estimate = .pred)

# A tibble: 3 × 3
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 mae     standard       0.523
# 2 rmse    standard       0.684
# 3 rsq     standard       0.323

# Genralization metrics are close to the training set metrics, 
# indicating that the model generalizes well.





