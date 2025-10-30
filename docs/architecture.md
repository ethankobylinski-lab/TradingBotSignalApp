# Portfolio Risk Lab Architecture

## Overview
The application is a single-file Shiny app (`SignalTrade.R`) that orchestrates
universe management, portfolio construction, and post-trade risk analytics for
interactive exploration. Supporting assets live alongside the app:

- `config/config.yml` captures API keys, file-system paths, refresh cadences, and
  governance toggles.
- `R/config_helpers.R` centralizes configuration loading with caching and
  type-safe accessors.
- `experiments/` holds serialized experiment runs captured via
  `log_experiment()` for auditability and reproducibility.
- `data/versions/` stores price snapshots hashed through
  `record_data_version()` so downstream analyses can be reproduced exactly.

## Runtime Flow
1. **Startup** – The app sources `R/config_helpers.R`, loads the YAML
   configuration, and merges it with sane defaults. Configuration attributes are
   propagated throughout helper functions for consistent behavior.
2. **User Input** – Users interact via a Shiny UI to choose tickers, risk
   profiles, and rebalancing controls. Symbol entry is normalized through a
   fuzzy-matching helper to guard against typos.
3. **Data Access** – Prices are retrieved from Yahoo Finance (`quantmod`) and
   cached in-memory for the session. Data-quality checks validate observation
   counts, missingness, duplicate timestamps, and stagnant series before
   analytics proceed.
4. **Portfolio Analytics** – Depending on the weighting mode, historical prices
   feed into a portfolio builder that returns log returns, performance tables,
   and visualization payloads.
5. **Risk Reporting** – The app summarizes VaR/ES statistics, drawdowns, rolling
   risk, and heuristics that drive the guidance widgets.
6. **Experiment Logging & Versioning** – When enabled, results are written to
   `experiments/<timestamp>/` alongside metadata (inputs, data-quality notes, and
   hashes). The same run persists a versioned snapshot under `data/versions/`
   using deterministic hashing to support reproducibility.

## Periodic Reporting
The configuration includes a `reporting` section pointing to an optional
RMarkdown template (e.g., `reports/weekly_overview.Rmd`). Teams can schedule the
command below via `cron` or any orchestrator to publish periodic summaries:

```sh
Rscript -e "rmarkdown::render('reports/weekly_overview.Rmd', output_dir = 'reports/output')"
```

This ties directly into the versioned datasets, ensuring published documents
reference immutable input hashes.

## Extensibility Notes
- Extend data-quality rules inside `run_data_quality_checks()` to incorporate
  volume, corporate actions, or vendor-specific anomalies.
- Modify `config/config.yml` to point to alternate data stores or adjust
  schedules without redeploying code.
- Additional experiment sinks (databases, object storage) can be wired into
  `log_experiment()` by reading supplemental configuration keys.
