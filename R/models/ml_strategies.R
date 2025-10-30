#' Machine learning driven strategies using tidymodels
#'
#' @param data Tibble with columns `date`, `symbol`, `price`, and engineered features.
#' @param params List of hyper-parameters controlling the model fit and allocation.
#' @return Tibble of `date`, `symbol`, `weight`, and `grade`.
#' @export
ml_elastic_net <- function(data, params = list()) {
  model_spec <- parsnip::linear_reg(
    penalty = params$penalty %||% 0.01,
    mixture = params$mixture %||% 0.5
  ) |>
    parsnip::set_engine("glmnet")
  train_ml_strategy(data, params, model_spec)
}

#' @export
ml_random_forest <- function(data, params = list()) {
  model_spec <- parsnip::rand_forest(
    mtry = params$mtry,
    trees = params$trees %||% 500,
    min_n = params$min_n %||% 5
  ) |>
    parsnip::set_engine("ranger") |>
    parsnip::set_mode("regression")
  train_ml_strategy(data, params, model_spec)
}

#' @export
ml_xgboost <- function(data, params = list()) {
  model_spec <- parsnip::boost_tree(
    trees = params$trees %||% 500,
    learn_rate = params$learn_rate %||% 0.05,
    tree_depth = params$tree_depth %||% 6,
    loss_reduction = params$loss_reduction %||% 0,
    stop_iter = params$stop_iter %||% 50
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")
  train_ml_strategy(data, params, model_spec)
}

train_ml_strategy <- function(data, params, model_spec) {
  dataset <- prepare_ml_dataset(data, params)
  if (nrow(dataset) < 10) {
    stop("Not enough data to train machine learning strategy.")
  }
  train_prop <- params$train_prop %||% 0.8
  split <- rsample::initial_time_split(dataset, prop = train_prop)

  recipe <- recipes::recipe(target ~ ., data = rsample::training(split)) |>
    recipes::update_role(date, symbol, new_role = "id") |>
    recipes::step_rm(date, symbol) |>
    recipes::step_impute_mean(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_predictors())

  workflow <- workflows::workflow() |>
    workflows::add_model(model_spec) |>
    workflows::add_recipe(recipe)

  fitted <- workflows::fit(workflow, data = rsample::training(split))
  predictions <- predict(fitted, new_data = dataset) |>
    dplyr::bind_cols(dataset[, c("date", "symbol")]) |>
    dplyr::rename(grade = .pred)

  allocation <- allocate_from_grade(predictions, params)
  tibble::as_tibble(allocation)
}

prepare_ml_dataset <- function(data, params) {
  checkmate::assert_data_frame(data)
  data <- dplyr::arrange(data, symbol, date)
  data <- dplyr::group_by(data, symbol) |>
    dplyr::mutate(target = dplyr::lead(log(price)) - log(price)) |>
    dplyr::ungroup()
  features <- params$features %||% identify_feature_columns(data)
  if (!length(features)) {
    stop("No feature columns detected for machine learning strategy.")
  }
  ds <- data |>
    dplyr::select(date, symbol, dplyr::all_of(features), target) |>
    tidyr::drop_na(target)
  ds
}

identify_feature_columns <- function(data) {
  base_cols <- c("date", "symbol", "price", "benchmark_price", "target")
  candidates <- setdiff(names(data), base_cols)
  keepers <- purrr::keep(candidates, ~ is.numeric(data[[.x]]))
  keepers
}

allocate_from_grade <- function(predictions, params) {
  params <- modifyList(list(top_n = 20L, max_weight = 1, threshold = 0), params)
  top_n <- params$top_n
  weight_per <- params$max_weight / max(top_n, 1)
  predictions |>
    dplyr::group_by(date) |>
    dplyr::mutate(
      rank = dplyr::dense_rank(dplyr::desc(grade)),
      weight = dplyr::if_else(rank <= top_n & grade > params$threshold, weight_per, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, symbol, weight, grade)
}
