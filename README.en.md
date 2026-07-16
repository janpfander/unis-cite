🇫🇷 [Français](README.md) | 🇬🇧 **English**

# Unis-Cité — Volunteer Report

This repository contains the data pipeline and the bilingual (French / English) report analyzing the
questionnaires that Unis-Cité volunteers fill out during their *service civique*.

Each volunteer is asked to answer three questionnaires: **Q1** at the beginning of their service,
**Q2** at the end, and **Q3** six months later. The report currently covers four cohorts
("promos"): 2020-21, 2021-22, 2022-23 and 2023-24. It describes who the volunteers are, how many
drop out, how their attitudes change over the year, how satisfied they are, how they see their own
future, and how the key programs differ from one another.

The rendered report is a website plus a PDF and a Word version, in French and in English.

**Everything in the report is a description of associations, not of causal effects.** The report says
so explicitly in its introduction, and any new section should keep that framing.

---

## 1. Getting set up

### What you need installed

- **R** (developed on 4.4.1) and **Quarto** (developed on 1.8.24).
- R packages: `tidyverse`, `readxl`, `labelled`, `sjlabelled`, `ggalluvial`, `sf`, `rmapshaper`,
  `wesanderson`, `kableExtra`, `broom`, `gtsummary`, `flextable`, `gt`, `ggpubr`, `DescTools`.
- A **LaTeX installation** for the PDF output (e.g. run `quarto install tinytex`).

### The `data/` folder — you have to create it yourself

**The data is not in this repository and never will be.** It contains personal information about
volunteers, so `data/` is listed in `.gitignore`. If you clone this repository you get the code and
no data, and nothing will render until you fix that.

Create a folder called `data/` at the root of the repository (next to `report.qmd`) and put the files
in it. You need to obtain them from Unis-Cité or from the previous maintainer:

```
unis-cite/
├── report.qmd
├── cleaning.qmd
└── data/                                  ← create this; it stays untracked
    ├── Promo 20-21.xlsx                   ← one file per cohort, this exact naming
    ├── Promo 21-22.xlsx
    ├── Promo 22-23.xlsx
    ├── Promo 23-24.xlsx
    ├── codebook.csv                       ← the hand-edited codebook (see §4.3)
    ├── Q1 2020-2021 - Trame des questions.pdf   ← the questionnaires as shown to volunteers
    ├── Q2 2020-2021 - Trame des questions.pdf
    ├── …                                        (one per wave per cohort)
    └── map/                               ← IGN shapefiles (see below)
        ├── COMMUNE.shp
        ├── DEPARTEMENT.shp
        └── REGION.shp
```

The filenames matter. `Promo XX-XX.xlsx` is hardcoded in `cleaning.qmd`, and the cohort label
(`promo`) is parsed out of the cleaned filenames with a regex, so a file named `Promo 2024-2025.xlsx`
will not be picked up.

The shapefiles for French administrative units are downloaded from
<https://geoservices.ign.fr/adminexpress>. They cover **metropolitan France only** — this matters,
see §5.

Running `cleaning.qmd` then generates the rest of the folder contents:
`cleaned_promo_XX-XX.csv` (one per cohort), `cleaned_promo_combined.csv`,
`cleaned_promo_combined.RData` (the object the report actually loads) and `map.rds`.

**Never commit anything from `data/`.**

### What's in each `Promo XX-XX.xlsx`

Five sheets, in this order:

| Position | Sheet name in Excel | Called in the code | What it holds |
| --- | --- | --- | --- |
| 1 | `Promo 20-21` | `promo_20` → source `q0` | The **administrative record** of each volunteer: `ID JEUNE`, sex, nationality, refugee status, age category, area of residence, education, disability, planned and actual duration of the service, reason for termination (`Motif de rupture`), programs 1 and 2. This is not a questionnaire — it's Unis-Cité's own file on the volunteer, and it is the source of every demographic variable in the report. |
| 2 | `Questions Q1` | `q1` | Questionnaire, beginning of the service. |
| 3 | `Questions Q2` | `q2` | Questionnaire, end of the service. |
| 4 | `Questions Q3` | `q3` | Questionnaire, six months later. |
| 5 | `Table de correspondances` | `programmes` | Read in, then **discarded** — it is not used anywhere in the analysis. |

**The sheets are matched by position, not by name.** The code reads whatever sheets exist and renames
them in order:

```r
better_names <- c("promo_20", "q1", "q2", "q3", "programmes")
names(all_sheets) <- better_names
```

So a new cohort's file can call its sheets whatever it likes, but if the order changes — or if a
sheet is added or removed — the wrong data gets the wrong name, and nothing will complain.

---

## 2. Repository structure

### The report itself

| File | What it is |
| --- | --- |
| `index.qmd` / `index.en.qmd` | The executive summary — the homepage. Pure prose, no analysis code. |
| `report.qmd` / `report.en.qmd` | The full report (~3,900 lines, mostly R chunks). |
| `tables.qmd` | Supplementary tables (demographics, repeated questions, geography, programs). |
| `codebook.qmd` | Renders `data/codebook.csv` as a browsable table. |

**French is the site's default language.** The plain filenames (`index.qmd`, `report.qmd`) are the
French versions and get the plain URLs (`report.html`, `report.pdf`); English lives at `*.en.*`.

**But the English file `report.en.qmd` is the content master.** The working convention is: make the
change in English first, then mirror it into `report.qmd` — translate the prose, keep the code
identical between the two. The same applies to the pair of executive summaries. When the two files
drift apart, the French version is the one that goes to the client, so drift is expensive.

The executive summaries contain **hardcoded numbers** copied from the report prose. They do not
recompute anything. If the underlying results change, you must update those numbers by hand in both
`index.qmd` and `index.en.qmd`.

### The pipeline and helpers

| File | What it is |
| --- | --- |
| `cleaning.qmd` | Raw Excel → cleaned CSVs → `cleaned_promo_combined.RData` + `map.rds`. **Excluded from the render**; run it by hand when the data changes. |
| `functions/functions.R` | Shared helpers used by the report chunks (see below). |
| `simulation.qmd` | Scratch file, also excluded from the render. |
| `language-toggle.html` | The EN/FR navbar toggle, injected into every HTML page. |
| `_quarto.yml`, `_variables.yml`, `styles.css` | Site config, footer variables, styling. |
| `_extensions/` | `apaquarto` (APA PDF/DOCX formats) and `fontawesome`. |
| `docs/` | The rendered output. It **is** committed — this is what GitHub Pages serves. |
| `codebook.csv` (repo root) | A tracked copy of `data/codebook.csv`, kept only so the codebook is versioned somewhere (since `data/` is gitignored). **Nothing reads it.** The live one is `data/codebook.csv`. |

Useful helpers in `functions/functions.R`:

- `run_regression()` — runs one simple regression per predictor and returns tidy results with a
  significance flag. This is the modeling workhorse of the whole report.
- `run_program_models()` — compares one key program against all other volunteers.
- `run_multivariable_model()` — used only in the multivariable appendix.
- `plot_within_change()`, `plot_vote_change()` — the per-promo alluvial + change-percentage plot pairs.
- `plot_faceted_distribution()` — the faceted distribution plots.
- `text_ready()`, `clean_t_test()`, `super_split()` — formatting helpers for inline prose.

---

## 3. How the report is built

The analysis code lives in the `.qmd` files and runs when you render. There is no separate build script.

```bash
# everything: both languages, HTML + PDF + DOCX
quarto render

# quick check of one file while working (a few minutes)
quarto render report.qmd --to html

# PDF + DOCX only (fast if the HTML is already rendered — freeze skips re-running R)
quarto render report.qmd --to apaquarto-pdf,apaquarto-docx
```

Output goes to `docs/`. `freeze: auto` means R chunks are only re-executed when the `.qmd` changes,
so the cached results live in `_freeze/`.

Three things worth knowing before you render:

1. **A project-wide `quarto render --to html` deletes the PDFs and DOCX files from `docs/`.** A
   project render prunes outputs it didn't produce. To refresh only the HTML, render single files,
   or re-render the PDFs afterwards with the command above.
2. **apaquarto emits `(W) Cannot find @sec-...` warnings. They are spurious** — the cross-references
   resolve fine in the actual outputs. Don't chase them.
3. **If you rename a `.qmd`, moving `_freeze/<name>` is not enough.** The execute-results JSON files
   inside embed `<name>_files/...` figure paths, which have to be rewritten to the new stem, or the
   rendered HTML will point at figure directories that don't exist.

The setup chunk (libraries, data load, `demographic_variables`, `demographic_variables_not_reported`,
`program_colours`) is **duplicated** across `report.qmd`, `report.en.qmd`, `tables.qmd` and
`codebook.qmd`. If you change one, change all four. (One known intentional difference: `tables.qmd`
and `codebook.qmd` list `motif_rupture` where the report files list `rupture`.)

---

## 4. Adding a new survey (a new promo)

This is the main maintenance task. Read section 5 on risks first — this pipeline is more fragile than
it looks, and most of the work is verification, not coding.

Budget a **full working day at minimum**, most of it spent on step 4.3.

### 4.1 — Get the data in place

Put the new cohort's export in `data/Promo 24-25.xlsx` (this exact naming pattern — the code depends
on it), with the five sheets in the order described in §1. Also save the questionnaire PDFs
(`Q1 2024-2025 - Trame des questions.pdf` etc.) — you will need them in step 4.3.

### 4.2 — Add a section to `cleaning.qmd`

Copy the whole `# Promo 2023-24` section and change the year everywhere. The block does five things:
read the sheets, name them, clean them against the codebook, drop the administrative columns from the
questionnaire sheets, join, and write `data/cleaned_promo_24-25.csv`.

**Watch out for the `promo_2X` object name.** Each cohort's section names its administrative sheet
after the cohort — `promo_20`, `promo_21`, `promo_22`, `promo_23` — and then refers to that name by
hand in five places: `better_names`, `vars_to_remove`, the `if (name != "promo_23")` guard, the
`cleaned_sheets$promo_23 <- NULL` line, and the `full_join(..., promo_23 |> select(-source), ...)`.
When you copy the section, all five have to become `promo_24`. Miss one and you get either an
"object not found" error (the lucky case) or a silently wrong join (the unlucky one).

Note that the *comments* in those chunks say "promo_20" in every section — they were copy-pasted and
never updated. Ignore them; read the code.

### 4.3 — Reconcile the new questions with `data/codebook.csv` (the hard part)

First, run the chunk that writes `data/codebook_prelimiary_24-25.csv`. It lists every question in the
new file, which wave(s) it appeared in, and flags questions that appear more than once.

Two reasons a question shows up twice, and you have to tell them apart **by hand**:

- The volunteer was enrolled in **two programs**, so they saw the same question twice within one wave.
  These are true duplicates.
- The question is a **follow-up to a different question** but happens to have the same wording
  (e.g. "Pour quelles raisons ?"). These are *not* duplicates and must not be merged.

The questionnaire PDFs are how you tell which is which.

Then comes the important part. `data/codebook.csv` maps each **question text** to a short
`variable_name`. The cleaning code does this, and only this:

```r
data |>
  select(any_of(codebook$question)) |>
  rename_with(~ codebook$variable_name[match(.x, codebook$question)], .cols = everything())
```

`any_of()` means: **a question whose wording does not match the codebook exactly is silently dropped.
No error, no warning.** If Unis-Cité changed "Sexe" to "Sexe :" between two years, that column simply
vanishes from the analysis, and the report will show the new cohort as all-missing on that variable —
which looks like a real finding rather than a bug.

So, for every question in the new cohort, decide one of three things:

- **Same wording as an existing codebook row** → nothing to do, it will map automatically.
- **Same question, new wording** → this is the dangerous case. Add a new codebook row with the *new*
  question text pointing at the *existing* `variable_name`, so both years land in the same column.
  Do not edit the old row — the old cohorts still need it.
- **Genuinely new question** → add a row with a new `variable_name`. It will not appear in the report
  until someone writes a section for it.

The codebook is maintained **by hand in Google Sheets** and re-imported as `data/codebook.csv`. Its
columns are `question`, `duplicate_flag`, `sources`, `variable_name`, `multiple_answers`,
`answer_options`. After you re-import it, copy it to the repo root as `codebook.csv` too, so the
change is versioned.

### 4.4 — Add the promo to the combined data

`# A common data frame` globs `data/cleaned_promo_\d{2}-\d{2}.csv`, so the new cohort is picked up
automatically and `promo` is parsed from the filename. But the map section has a **hardcoded list**:

```r
promos <- c("20-21", "21-22", "22-23", "23-24")   # cleaning.qmd, ~line 1165
```

Add the new promo there, or it will be missing from every map.

### 4.5 — Check the recoding sections

Everything after `## Rupture variable` in `cleaning.qmd` recodes raw French answer strings into
analysis variables, by matching those strings **literally**. For example:

```r
satisfaction = factor(satisfaction, levels = c(
  "Pas du tout satisfaisante", "Peu satisfaisante",
  "Assez satisfaisante", "Très satisfaisante"))
```

If the new cohort writes "Très satisfaisant" (no final *e*), or the administrative file uses a new
`Motif de rupture` code, those answers become `NA` — again, silently.

Go through each recoding section (`rupture`, `type_volontaire`, `satisfaction`, `confiance_en_soi`,
`confiance_avenir_personnel`, `comparaison_utile_autres`, `fierte`, `confiance_avenir`,
`individual action`, `zone_residence`, `education`, `sex`, `age`, `refugie`, `programme_grouped`,
`key programs`) and check the new cohort's values against it. The fastest check is a table per
variable:

```r
combined_data |> count(promo, satisfaction)   # any promo with unexpected NA is a wording mismatch
```

The two program variables deserve particular attention, and they fail in different ways:

- `programme_grouped` (broad categories: Aidance, Culture, Autre…) is built by joining a **hardcoded
  `tribble` that lists every program name exactly**. A program name that isn't in that table — new,
  renamed, or just spelled differently — produces `NA`.
- `programme_cle` (the seven key programs) is built with **regexes** on `programme_1`
  (`str_detect(programme_1, regex("Solidarité Aidants", ignore_case = TRUE))`). These are more
  forgiving of small variations, but a renamed program still falls through to `NA` and drops out of
  the program comparisons.

### 4.6 — Rebuild the map data

If the new cohort has any new `site` value, the map section needs attention:

- Site names are cleaned by hand (`recode(site, "Saint-Etienne" = "Saint-Étienne", ...)`).
- Sites in **La Réunion are excluded** — the IGN shapefiles only cover metropolitan France.
- **Several French communes share a name** (there is a Valence in the Drôme, the Charente and the
  Tarn-et-Garonne). Ambiguous names are resolved by hand in the `ambiguous_sites` table, verified
  against the `region` variable. If a new site name is ambiguous and you don't add it there, its
  volunteers get counted in several départements at once.

The check chunk right after `commune_matches` must return **zero rows**. Sites matching neither a
commune nor a département are silently dropped, so run the anti-join chunk and see what falls out.

Then regenerate `data/map.rds`.

### 4.7 — Write out the data

Run the `# Write out data` chunk to refresh `data/cleaned_promo_combined.csv` and
`data/cleaned_promo_combined.RData`. The report reads the `.RData`.

### 4.8 — Update the report

Most of the report loops over promos and picks up the new cohort automatically. What does **not**:

- **Hardcoded promo names** — about 16 occurrences in each of `report.qmd` and `report.en.qmd`.
  Find them with `grep -n '23-24' report.en.qmd report.qmd`. They include:
  - the trend maps, which compute `` `23-24` - `20-21` `` — decide whether the trend should now run
    to the new cohort;
  - the per-promo sections of the voting and individual-action analyses (`### Promo 2023-24`), each
    with its own figure, `promo_filter = "23-24"`, caption and prose. A new cohort needs a new
    subsection copied from these, plus its own interpretation;
  - prose numbers throughout ("from 13% in 2020-21 to 24% in 2023-24") — these are written by hand
    and will be **wrong but plausible-looking** if you don't revisit them.
- **The executive summaries** — every number in `index.qmd` / `index.en.qmd` is hardcoded.
- The voting section's prose relates each cohort to the **actual elections** that fell in its service
  year. A new cohort needs that context researched and written.

Remember: English first, then mirror into French. For mechanical edits across both files, the
established approach is a small Python script using literal string replacements plus count
assertions, so a silent no-op replacement can't slip through.

### 4.9 — Render and verify

```bash
quarto render
```

Then actually look at the output. The failure mode of this pipeline is not a crash, it's a chart that
renders beautifully with a whole year of data quietly missing. Check specifically:

- every variable has sensible non-missing counts for the **new** promo (`count(promo, <var>)`);
- response rates in the attrition section are plausible for the new cohort;
- the new cohort appears on the maps, and the total volunteer count matches what Unis-Cité expects;
- the key-program comparisons still include all seven programs;
- the cross-references and figures resolve in the PDF, not just the HTML.

---

## 5. Risks and limitations — please read

**This pipeline fails silently.** That is the single most important thing to know about it. There are
four places where a mismatch produces missing data rather than an error:

1. `select(any_of(codebook$question))` — a question whose wording changed is **dropped without warning**.
2. The recoding sections match French answer strings literally — a changed answer option becomes `NA`.
3. The `programme_grouped` lookup table — an unlisted program name becomes `NA`.
4. The sheets of each Excel file are matched **by position** — a reordered file mislabels everything.

In all four cases the report still renders, and the new cohort just looks like it has a lot of
missing data or an interesting new pattern. **Never assume a successful render means correct data.**
Always compare counts per promo before believing a result.

Other things to keep in mind:

- **The codebook is hand-maintained**, in Google Sheets, outside this repository. It is the real
  interface between the survey and the analysis, and it has no tests. Treat edits to it with the same
  care as edits to code.
- **Duplicate questions are identified by hand.** Volunteers in two programs answer some questions
  twice; some same-worded questions are distinct follow-ups. The automatic flag over-detects, and
  someone has to read the questionnaire PDFs to resolve it.
- **Duplicate rows are resolved by keeping the first occurrence.** Where a volunteer appears twice in
  a wave, `slice(1)` picks the first — even where the two rows have genuinely different answers. This
  was a pragmatic decision made without better information.
- **The maps cover metropolitan France only.** La Réunion volunteers are excluded by hand, and their
  site names ("Saint-Denis", "Saint-Pierre", "Saint-Benoît") also exist in metropolitan France, so
  the exclusion list is name-based and was verified against `region`. Adding a cohort with overseas
  sites requires redoing that check.
- **The report's numbers in prose are hardcoded.** Inline results computed from the data update
  themselves; sentences like "48% in 2020-21" do not. After any change to the data, re-read the prose.
- **The modeling is deliberately descriptive**: one simple (logistic) regression per predictor,
  significance-flagged, shown as forest-style plots and appendix tables. This is a choice, not an
  oversight. There is a multivariable appendix, but the main text is univariate throughout. Anything
  new should follow the same pattern rather than introducing a different modeling approach.
- **Everything is an association.** No causal claims, anywhere. In particular, differences between
  programs reflect who signs up for them as much as what happens in them.
- **`data/` must stay out of git.** It contains personal data.

### Style conventions worth keeping

- Report prose names groups and directions explicitly ("volunteers without a bac reported lower
  satisfaction than…"), rather than saying "X predicts Y".
- The audience is non-technical. Statistics are explained in plain language, with concrete examples.
- Sections follow a repeating shape: overview → trend across promos → predictors → key-program
  differences. New sections should match it.
- When you touch code that is duplicated across chunks, factor it into `functions/functions.R`
  instead of patching the copies. Verify a refactor by re-rendering and comparing figure hashes
  (`md5 -q docs/index_files/figure-html/<fig>.png`).
- French labels appear inside the English figures, because the data values are the French answer
  options. That is expected, not a bug.
- New bilingual pages must be added to the `pairs` map in `language-toggle.html`. That file does
  **link rewriting only, never redirects** — an earlier redirect-based version looped when
  localStorage was unavailable (e.g. Safari private windows).

---

## 6. Contact

The report was written by Jan Pfänder (janlukas.pfaender@gmail.com). Source:
<https://github.com/janpfander/unis-cite>.
