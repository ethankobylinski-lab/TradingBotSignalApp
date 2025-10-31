.ensure_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for configuration loading.", pkg), call. = FALSE)
  }
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

.load_config_cache <- local({
  cfg <- NULL
  path <- NULL
  list(
    get = function() cfg,
    set = function(value, config_path) {
      cfg <<- value
      path <<- config_path
    },
    path = function() path
  )
})

load_app_config <- function(config_path = "config/config.yml", reload = FALSE) {
  .ensure_namespace("yaml")

  cached <- .load_config_cache$get()
  if (!reload && !is.null(cached) && identical(normalizePath(.load_config_cache$path()), normalizePath(config_path))) {
    return(cached)
  }

  if (!file.exists(config_path)) {
    stop(sprintf("Configuration file not found at '%s'.", config_path), call. = FALSE)
  }

  cfg <- yaml::read_yaml(config_path)
  attr(cfg, "config_path") <- normalizePath(config_path)
  .load_config_cache$set(cfg, config_path)
  cfg
}

get_config_value <- function(path, default = NULL, config = load_app_config()) {
  if (!length(path)) {
    return(default)
  }

  current <- config
  for (name in path) {
    if (is.list(current) && !is.null(current[[name]])) {
      current <- current[[name]]
    } else {
      return(default)
    }
  }
  current
}

get_config_path <- function(...) {
  config <- load_app_config()
  value <- get_config_value(list(...), default = NULL, config = config)
  if (is.null(value)) {
    return(NULL)
  }
  normalizePath(value, mustWork = FALSE)
}
