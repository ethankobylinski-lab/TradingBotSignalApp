#' Cross-sectional factor scoring strategy.
#'
#' @param data Tibble including `date`, `symbol`, `price`, and factor columns.
#' @param params List with optional elements `factors`, `top_n`, and `max_weight`.
#' @export
factor_rank_score <- function(data, params = list()) {
  checkmate::assert_data_frame(data)
  params <- modifyList(list(factors = NULL, top_n = 10L, max_weight = 1), params)
  base_cols <- c("date", "symbol", "price", "benchmark_price")
  available_factors <- setdiff(names(data), base_cols)
  factors <- params$factors %||% available_factors
  if (!length(factors)) {
    stop("No factor columns detected. Provide `params$factors` explicitly.")
  }
  if (!all(factors %in% names(data))) {
    stop("The following factors are missing from data: ", paste(setdiff(factors, names(data)), collapse = ", "))
  }
  numeric_factors <- purrr::keep(factors, ~is.numeric(data[[.x]]))
  if (!length(numeric_factors)) {
    stop("Selected factors must be numeric columns.")
  }
  top_n <- min(params$top_n, length(unique(data$symbol)))
  weight_per_name <- params$max_weight / max(top_n, 1)

  signals <- data |>
    dplyr::group_by(date) |>
    dplyr::arrange(symbol, .by_group = TRUE) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(numeric_factors), ~ as.numeric(scale(.x)), .names = "scaled_{.col}"),
      grade = rowMeans(dplyr::across(dplyr::starts_with("scaled_")), na.rm = TRUE)
    ) |>
    dplyr::mutate(rank = dplyr::dense_rank(dplyr::desc(grade))) |>
    dplyr::mutate(weight = dplyr::if_else(rank <= top_n & grade > 0, weight_per_name, 0)) |>
    dplyr::ungroup() |>
    dplyr::select(date, symbol, weight, grade)
  signals$weight[is.na(signals$weight)] <- 0
  signals$grade[is.na(signals$grade)] <- 0
  tibble::as_tibble(signals)
}
