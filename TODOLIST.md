Title: Add conflict intensity lag as instrument (from Bohra-Mishra 2011)

Description:
From @BohraMishra2011 - they use lagged conflict intensity as IV for current migration decisions.

**To implement:**
- [ ] Calculate district-level conflict intensity from INSEC data
- [ ] Create 1-year and 2-year lags
- [ ] Test weak instrument diagnostics
- [ ] Compare with my current specification

**Relevant code location:** `code/analysis/migration_models.R`
**Related to:** Chapter 2 - Conflict and Migration
**Priority:** High
**Estimated time:** 2-3 days

**Notes:**
- Their F-stat was 18.4, I should aim for >10
- See their Table 3 for specification