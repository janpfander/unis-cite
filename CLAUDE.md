# Unis-Cité Volunteer Report — Project Context

## What this is

A consulting project analyzing survey data for **Unis-Cité**, a French NGO organizing the *service civique*. The deliverable is a bilingual (EN/FR) Quarto website + PDF/DOCX report analyzing questionnaires from four volunteer cohorts ("promos": 2020-21, 2021-22, 2022-23, 2023-24). Each volunteer was asked to fill out three questionnaires (q1, q2, q3) over their service year.

**Naming:** The NGO's real name "Unis-Cité" is used throughout (an earlier masking as "Cité-Unis" was dropped on 2026-07-07 per Jan). `mask: true` in the report YAML is the apaquarto author-masking option, unrelated. Raw data is gitignored (`data/`) — never commit anything from `data/`.

## Repository layout

- `index.qmd` (**French**) / `index.en.qmd` (English) — bilingual executive summary; the homepage is the French version (French is the site's default language). Pure prose, no R chunks; hardcoded numbers taken from the report prose — update by hand if the underlying results change. Links into report sections via `report.fr.qmd#sec-...` / `report.qmd#sec-...`.
- `report.qmd` — the main report (English; formerly `index.qmd`). ~3900 lines, mostly R chunks.
- `report.fr.qmd` — full French translation of `report.qmd`. **Kept in sync manually**: code chunks are identical, only prose/captions are translated. Any change to `report.qmd` must be mirrored here.
- `language-toggle.html` — JS included after body on every HTML page (via `_quarto.yml`). **Never redirects — link rewriting only** (a redirect-based version looped when localStorage was blocked, e.g. Safari private windows). "Mode" = language of the last bilingual page visited (localStorage `site-lang`, default French); navbar links and the injected navbar toggle follow the mode. New bilingual pages must be added to its `pairs` map.
- `tables.qmd` — supplementary tables (demographics via `gtsummary`, repeated questions, geographic trends, programs).
- `codebook.qmd` — renders `data/codebook.csv` as a table.
- `cleaning.qmd` — data cleaning pipeline (raw Excel → cleaned CSVs → `cleaned_promo_combined.RData`). **Excluded from render** in `_quarto.yml`, run manually when data changes.
- `simulation.qmd` — also excluded from render.
- `functions/functions.R` — helpers: `run_regression()` (one model per predictor, returns tidy results + significance flag), `run_program_models()`, `plot_faceted_distribution()`, `plot_within_change()` (per-promo alluvial + change-percentage plot pair, used 4× in the individual-action section of both report files), `text_ready()`, `clean_t_test()`, `super_split()`.
- `data/` (gitignored) — raw `Promo XX-XX.xlsx` per cohort, questionnaire PDFs ("Trame des questions"), cleaned CSVs, `cleaned_promo_combined.RData` (loads as `combined_data`), `map.rds` (sf object for département maps), `codebook.csv`.
- `docs/` — rendered site output (committed; GitHub Pages style with `.nojekyll`).
- `_extensions/` — `wjschne/apaquarto` (APA formats for PDF/DOCX), `quarto-ext/fontawesome`.

## Build

- Quarto website project, `output-dir: docs`, `freeze: auto`, `echo: false`.
- Render: `quarto render` (renders `index.qmd`, `index.fr.qmd`, `report.qmd`, `report.fr.qmd`, `tables.qmd`, `codebook.qmd`).
- `report.qmd` / `report.fr.qmd` output three formats: `html`, `apaquarto-docx`, `apaquarto-pdf` (documentmode: doc).
- R packages: tidyverse, readxl, labelled, sjlabelled, ggalluvial, sf, rmapshaper, wesanderson, kableExtra, broom, gtsummary, flextable, gt, ggpubr, DescTools. (`brms` commented out everywhere — Bayesian models were drafted but not used.)

## Data pipeline

1. Raw Google-survey exports per cohort (`Promo XX-XX.xlsx`, one sheet per wave: promo roster, q1, q2, q3, programmes).
2. `cleaning.qmd` builds a preliminary codebook per cohort; the codebook is then **hand-edited in Google Sheets** (variable selection, naming, duplicate removal, answer options) and re-imported as `data/codebook.csv`.
3. Cleaning uses the codebook to rename/select; output is `cleaned_promo_combined.RData` → object `combined_data`, long by wave (`source` column: q0/q1/q2/q3; `promo` column for cohort).
4. Duplicate-question caveat: volunteers in two programs saw some questions twice; identified by hand in the codebook.

## Report structure (report.qmd)

1. **Introduction** — explicitly frames all results as associations, *not* causal.
2. **Who are the volunteers?** (`#sec-who`) — geography (sf maps by département), age, education, sex.
3. **Attrition** (`#sec-attrition`) — response rates per wave, with/without ruptures.
4. **Attitude change** (`#sec-within-change`) — voting (`#sec-vote`), individual action for society (`#sec-individual-action`). McNemar tests + alluvial plots (ggalluvial) for within-person change between waves; per-demographic logistic regressions on change.
5. **Rupture** (`#sec-rupture`) — early contract termination; trends and predictors.
6. **Satisfaction** (`#sec-satisfaction`) — same pattern (trend, change, predictors, key-program differences).
7. **Confidence in one's future** (`#sec-perception-avenir`) — same pattern.
8. **Differences between programs** (`#sec-programs`) — key programs: Solidarité Aidants, Cinéma & Citoyenneté, Booster, Ecovolonterre, Médiaterre, ASM, Solidarité Séniors. Each compared against all other volunteers via `run_program_models()`.
9. **Conclusion** — methodological notes for future questionnaires.
10. **Appendix** — regression tables per outcome.

## Conventions & gotchas

- The setup chunk (libraries, data load, `demographic_variables`, `demographic_variables_not_reported`, `program_colours`) is **duplicated** across `report.qmd`, `report.fr.qmd`, `tables.qmd`, `codebook.qmd` — keep them consistent when editing. (Note: `tables.qmd`/`codebook.qmd` use `motif_rupture` in `demographic_variables` where `report.qmd` uses `rupture`.)
- Modeling approach throughout: one simple (logistic) regression per predictor, significance-flagged, displayed via forest-style plots and appendix tables — deliberately descriptive, not multivariate.
- Repeated plot pattern: paired absolute-numbers + percentages plots with `fig-subcap` and `layout-ncol: 2`.
- `program_colours`: 7-color custom palette defined in the setup chunk; `wesanderson` also used.
- French labels appear inside "English" figures (data values are French answer options) — that's expected.
- Workflow for translations so far: separate branch (`index-fr`, `index-fr-new`) + PR into `main`.
- Site text/footer variables in `_variables.yml` (years, github-url, orcid).

## Working practices (per Jan)

- Keep code parsimonious: when touching duplicated chunk code, factor it into `functions/functions.R` instead of patching copies. Verify refactors by re-rendering and hash-comparing the output figures (`md5 -q docs/index_files/figure-html/<fig>.png`).
- `report.qmd` (EN) is the master; mirror every change into `report.fr.qmd` (translate prose, keep code identical). Bulk mechanical edits across both files are done with Python scripts using literal replacements + count assertions. The executive summaries (`index.qmd` FR / `index.en.qmd` EN) are likewise a pair to keep in sync.
- Quick verification render: `quarto render report.qmd --to html` (a few minutes). PDF/DOCX only via full render before submissions.
- Report sections follow a repeating pattern (overview → trend across promos → predictors → key-program differences); new sections should match it.

## Status (2026-07-07)

Site restructured: report moved to `report.qmd`/`report.fr.qmd` (freeze caches moved with `git mv`, no re-execution), new bilingual executive-summary homepage (French `index.qmd` is the site root, English at `index.en.qmd`), navbar EN/FR toggle via `language-toggle.html` (link rewriting only, no redirects). Full render done (all formats, both languages); cross-refs verified in HTML and PDFs. All uncommitted on `main`.

Gotchas learned: (1) when renaming a `.qmd`, moving `_freeze/<name>` is not enough — the execute-results JSONs embed `<name>_files/...` figure paths, which must be sed-rewritten to the new stem, or the rendered HTML points at nonexistent figure dirs. (2) apaquarto renders emit `(W) Cannot find @sec-...` warnings that are spurious — refs resolve fine in the outputs. (3) A project-wide `quarto render --to html` **deletes** the PDF/DOCX outputs from `docs/` (project renders prune outputs they didn't produce); to refresh HTML without losing them, render single files, or re-render PDFs afterwards via `quarto render report.qmd --to apaquarto-pdf,apaquarto-docx` (fast thanks to freeze, only LaTeX/pandoc re-run).

## Status (2026-07-06)

March/May 2026 team feedback fully addressed in both language files (see memory for detail); satisfaction-composition analysis added (§sec-satisfaction-composition); full FR caption + prose quality pass done. Full accuracy review done later the same day: map join fixed in `cleaning.qmd` (same-named communes disambiguated by hand, La Réunion excluded — metro-only shapefile; `map.rds` regenerated), rupture-model `Duree_reelle_sc_mois` capitalization bug fixed, wrong "~70% voting stability" claim corrected, plus new sections in both files: §sec-attrition-bias, fig-rupture-timing, §sec-appendix-multivariable (helpers `plot_vote_change()` and `run_multivariable_model()` added to functions.R; the four per-promo vote chunks now call `plot_vote_change()`). HTML rendered; PDF/DOCX renders pending; everything uncommitted on `main`.
