# Compliance & Governance Controls

## Policy Overview
The TradingBot Signal application is subject to internal risk-management and
regulatory policies. The configuration file (`config/config.yml`) exposes the
main enforcement levers so compliance teams can update behavior without code
changes.

### Key Flags
- `compliance.enforce_pii_scrub` – Ensures sensitive identifiers are removed
  from downstream exports prior to persistence or sharing.
- `compliance.enable_audit_logging` – Activates experiment logging via
  `log_experiment()` so every analytical run is captured with metadata, hash
  references, and timestamps.
- `compliance.retention_days` – Drives archival routines that purge experiment
  artifacts after the stated retention window.
- `compliance.export_controls` – Optional geo-fencing switches to prevent
  distribution to disallowed regions.

## Data Governance
1. **Data Quality** – `run_data_quality_checks()` enforces minimum observation
   counts, missingness tolerance, duplicate timestamp detection, and stagnant
   price warnings. Results are attached to every experiment log to provide
   visibility during audits.
2. **Version Control** – `record_data_version()` snapshots the pricing inputs and
   stores a deterministic hash plus metadata inside `data/versions/`. Recreating
   an analysis requires only the corresponding RDS and the recorded hash.
3. **Experiment Logging** – Runs are serialized under `experiments/<timestamp>/`
   with both RDS payloads and YAML metadata. When `experiments.redact_inputs` is
   enabled, inputs are obfuscated prior to persistence.
4. **Reporting** – The `reporting` block in the configuration encourages the use
   of scheduled RMarkdown reports. Generated PDFs or HTML exports must respect
   the same retention policy and hash references for traceability.

## Operational Checklist
- Review API keys and tokens quarterly; rotate credentials following security
  policy and update `config/config.yml` accordingly.
- Validate that directories defined in `data` exist with restricted access
  controls and appropriate backup coverage.
- Monitor automated jobs (price refresh, compliance reporting, experiment
  clean-up) to verify they align with defined cron expressions.
- During audits, provide the `metadata.yml` + snapshot hash for any run to prove
  lineage from source data to reported metrics.
