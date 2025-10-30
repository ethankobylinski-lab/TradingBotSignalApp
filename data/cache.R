cache_dir <- file.path("data", "_cache")
.cache_ext <- if (requireNamespace("qs", quietly = TRUE)) "qs" else "rds"

.ensure_cache_dir <- function() {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(cache_dir)
}

cache_path <- function(key, ext = .cache_ext) {
  .ensure_cache_dir()
  safe_key <- gsub("[^A-Za-z0-9_-]+", "_", key)
  file.path(cache_dir, paste0(safe_key, ".", ext))
}

cache_is_fresh <- function(path, max_age = Inf) {
  if (!file.exists(path)) return(FALSE)
  if (is.infinite(max_age)) return(TRUE)
  info <- file.info(path)
  if (is.na(info$mtime)) return(FALSE)
  age <- difftime(Sys.time(), info$mtime, units = "secs")
  age <= as.numeric(max_age, units = "secs")
}

cache_save <- function(key, value, ext = .cache_ext) {
  path <- cache_path(key, ext)
  if (.cache_ext == "qs") {
    qs::qsave(value, path)
  } else {
    saveRDS(value, path)
  }
  invisible(path)
}

cache_load <- function(key, max_age = Inf, ext = .cache_ext) {
  path <- cache_path(key, ext)
  if (!file.exists(path)) return(NULL)
  if (!cache_is_fresh(path, max_age)) return(NULL)
  if (.cache_ext == "qs") {
    qs::qread(path)
  } else {
    readRDS(path)
  }
}

with_cache <- function(key, max_age = Inf, expr, ext = .cache_ext) {
  cached <- cache_load(key, max_age, ext)
  if (!is.null(cached)) return(cached)
  value <- force(expr)
  if (!is.null(value)) {
    cache_save(key, value, ext)
  }
  value
}
