🇫🇷 **Français** | 🇬🇧 [English](README.en.md)

# Unis-Cité — Rapport Volontaires

Ce dépôt contient le pipeline de données et le rapport bilingue (français / anglais) analysant les
questionnaires que les volontaires d'Unis-Cité remplissent au cours de leur service civique.

Il est demandé à chaque volontaire de répondre à trois questionnaires : **Q1** au début du service,
**Q2** à la fin, et **Q3** six mois plus tard. Le rapport couvre actuellement quatre cohortes
(« promos ») : 2020-21, 2021-22, 2022-23 et 2023-24. Il décrit qui sont les volontaires, combien
d'entre eux interrompent leur service, comment leurs attitudes évoluent au cours de l'année, leur
satisfaction, la confiance qu'ils ont en leur avenir, et ce qui distingue les programmes clés les uns
des autres.

Le rapport se décline en un site web, ainsi qu'une version PDF et une version Word, en français et en
anglais.

**Tout ce que présente le rapport relève de l'association, et non de l'effet causal.** Le rapport le
dit explicitement dans son introduction, et toute nouvelle section doit conserver ce cadrage.

---

## 1. Installation

### Ce qu'il faut installer

- **R** (développé avec la version 4.4.1) et **Quarto** (développé avec la version 1.8.24).
- Les packages R : `tidyverse`, `readxl`, `labelled`, `sjlabelled`, `ggalluvial`, `sf`, `rmapshaper`,
  `wesanderson`, `kableExtra`, `broom`, `gtsummary`, `flextable`, `gt`, `ggpubr`, `DescTools`.
- Une **installation LaTeX** pour la sortie PDF (par ex. via `quarto install tinytex`).

### Le dossier `data/` — à créer soi-même

**Les données ne se trouvent pas dans ce dépôt et ne s'y trouveront jamais.** Elles contiennent des
informations personnelles sur les volontaires ; `data/` figure donc dans le `.gitignore`. En clonant
ce dépôt, vous obtenez le code et aucune donnée, et rien ne pourra être généré tant que vous n'y avez
pas remédié.

Créez un dossier nommé `data/` à la racine du dépôt (à côté de `report.qmd`) et déposez-y les
fichiers. Il faut les obtenir auprès d'Unis-Cité ou de la personne qui maintenait le projet
auparavant :

```
unis-cite/
├── report.qmd
├── cleaning.qmd
└── data/                                  ← à créer ; reste non suivi par git
    ├── Promo 20-21.xlsx                   ← un fichier par cohorte, exactement ce nommage
    ├── Promo 21-22.xlsx
    ├── Promo 22-23.xlsx
    ├── Promo 23-24.xlsx
    ├── codebook.csv                       ← le codebook édité à la main (voir §4.3)
    ├── Q1 2020-2021 - Trame des questions.pdf   ← les questionnaires tels que présentés
    ├── Q2 2020-2021 - Trame des questions.pdf
    ├── …                                        (un par vague et par cohorte)
    └── map/                               ← fonds de carte IGN (voir ci-dessous)
        ├── COMMUNE.shp
        ├── DEPARTEMENT.shp
        └── REGION.shp
```

Les noms de fichiers comptent. `Promo XX-XX.xlsx` est écrit en dur dans `cleaning.qmd`, et
l'identifiant de cohorte (`promo`) est extrait du nom des fichiers nettoyés par une expression
régulière : un fichier nommé `Promo 2024-2025.xlsx` ne sera donc pas pris en compte.

Les fonds de carte des unités administratives françaises proviennent de
<https://geoservices.ign.fr/adminexpress>. Ils ne couvrent **que la France métropolitaine** — ce point
a son importance, voir §5.

L'exécution de `cleaning.qmd` génère ensuite le reste du contenu du dossier :
`cleaned_promo_XX-XX.csv` (un par cohorte), `cleaned_promo_combined.csv`,
`cleaned_promo_combined.RData` (l'objet effectivement chargé par le rapport) et `map.rds`.

**Ne jamais committer quoi que ce soit provenant de `data/`.**

### Ce que contient chaque `Promo XX-XX.xlsx`

Cinq feuilles, dans cet ordre :

| Position | Nom de la feuille dans Excel | Nom dans le code | Contenu |
| --- | --- | --- | --- |
| 1 | `Promo 20-21` | `promo_20` → source `q0` | Le **dossier administratif** de chaque volontaire : `ID JEUNE`, sexe, nationalité, statut de réfugié, catégorie d'âge, zone de résidence, niveau d'études, handicap, durée prévue et réelle du service, motif de rupture, programmes 1 et 2. Ce n'est pas un questionnaire : c'est le fichier d'Unis-Cité sur le volontaire, et la source de toutes les variables démographiques du rapport. |
| 2 | `Questions Q1` | `q1` | Questionnaire, début du service. |
| 3 | `Questions Q2` | `q2` | Questionnaire, fin du service. |
| 4 | `Questions Q3` | `q3` | Questionnaire, six mois plus tard. |
| 5 | `Table de correspondances` | `programmes` | Lue, puis **écartée** — elle n'est utilisée nulle part dans l'analyse. |

**Les feuilles sont identifiées par leur position, et non par leur nom.** Le code lit les feuilles
telles qu'elles se présentent et les renomme dans l'ordre :

```r
better_names <- c("promo_20", "q1", "q2", "q3", "programmes")
names(all_sheets) <- better_names
```

Le fichier d'une nouvelle cohorte peut donc nommer ses feuilles comme il l'entend ; en revanche, si
l'ordre change — ou si une feuille est ajoutée ou supprimée — les mauvaises données reçoivent les
mauvais noms, sans que rien ne le signale.

---

## 2. Structure du dépôt

### Le rapport lui-même

| Fichier | Rôle |
| --- | --- |
| `index.qmd` / `index.en.qmd` | Le résumé exécutif — la page d'accueil. Uniquement de la prose, aucun code d'analyse. |
| `report.qmd` / `report.en.qmd` | Le rapport complet (~3 900 lignes, essentiellement des chunks R). |
| `tables.qmd` | Tableaux supplémentaires (démographie, questions répétées, géographie, programmes). |
| `codebook.qmd` | Affiche `data/codebook.csv` sous forme de tableau consultable. |

**Le français est la langue par défaut du site.** Les noms de fichiers simples (`index.qmd`,
`report.qmd`) correspondent aux versions françaises et obtiennent les URL simples (`report.html`,
`report.pdf`) ; l'anglais vit dans les fichiers `*.en.*`.

**Mais le fichier anglais `report.en.qmd` fait référence pour le contenu.** La convention de travail
est la suivante : effectuer la modification d'abord en anglais, puis la répercuter dans `report.qmd`
— traduire la prose, garder le code identique entre les deux. Il en va de même pour la paire de
résumés exécutifs. Quand les deux fichiers divergent, c'est la version française qui part chez le
client : la divergence coûte donc cher.

Les résumés exécutifs contiennent des **chiffres écrits en dur**, recopiés depuis la prose du
rapport. Ils ne recalculent rien. Si les résultats sous-jacents changent, il faut mettre ces chiffres
à jour à la main, dans `index.qmd` **et** dans `index.en.qmd`.

### Le pipeline et les fonctions

| Fichier | Rôle |
| --- | --- |
| `cleaning.qmd` | Excel brut → CSV nettoyés → `cleaned_promo_combined.RData` + `map.rds`. **Exclu du rendu** ; à exécuter à la main quand les données changent. |
| `functions/functions.R` | Fonctions partagées utilisées par les chunks du rapport (voir ci-dessous). |
| `simulation.qmd` | Fichier de travail, également exclu du rendu. |
| `language-toggle.html` | Le sélecteur EN/FR de la barre de navigation, injecté dans chaque page HTML. |
| `_quarto.yml`, `_variables.yml`, `styles.css` | Configuration du site, variables de pied de page, styles. |
| `_extensions/` | `apaquarto` (formats APA PDF/DOCX) et `fontawesome`. |
| `docs/` | La sortie générée. Elle **est** versionnée — c'est ce que sert GitHub Pages. |
| `codebook.csv` (racine du dépôt) | Une copie versionnée de `data/codebook.csv`, conservée uniquement pour que le codebook soit tracé quelque part (puisque `data/` est ignoré par git). **Aucun fichier ne le lit.** Le fichier vivant est `data/codebook.csv`. |

Fonctions utiles dans `functions/functions.R` :

- `run_regression()` — lance une régression simple par prédicteur et renvoie des résultats mis en
  forme avec un indicateur de significativité. C'est le moteur de modélisation de tout le rapport.
- `run_program_models()` — compare un programme clé à l'ensemble des autres volontaires.
- `run_multivariable_model()` — utilisée uniquement dans l'annexe multivariée.
- `plot_within_change()`, `plot_vote_change()` — les paires de graphiques alluviaux + pourcentages de
  changement, par promo.
- `plot_faceted_distribution()` — les graphiques de distribution en facettes.
- `text_ready()`, `clean_t_test()`, `super_split()` — fonctions de mise en forme pour la prose.

---

## 3. Comment le rapport est généré

Le code d'analyse vit dans les fichiers `.qmd` et s'exécute au moment du rendu. Il n'y a pas de script
de build séparé.

```bash
# tout : les deux langues, HTML + PDF + DOCX
quarto render

# vérification rapide d'un seul fichier pendant le travail (quelques minutes)
quarto render report.qmd --to html

# PDF + DOCX uniquement (rapide si le HTML est déjà généré — freeze évite de relancer R)
quarto render report.qmd --to apaquarto-pdf,apaquarto-docx
```

La sortie va dans `docs/`. `freeze: auto` signifie que les chunks R ne sont ré-exécutés que lorsque le
`.qmd` change ; les résultats en cache vivent dans `_freeze/`.

Trois choses à savoir avant de lancer un rendu :

1. **Un `quarto render --to html` sur l'ensemble du projet supprime les PDF et les DOCX de `docs/`.**
   Un rendu de projet élimine les sorties qu'il n'a pas produites. Pour ne rafraîchir que le HTML,
   rendez les fichiers un par un, ou regénérez ensuite les PDF avec la commande ci-dessus.
2. **apaquarto émet des avertissements `(W) Cannot find @sec-...`. Ils sont sans objet** — les
   renvois se résolvent correctement dans les sorties. Inutile de les poursuivre.
3. **Si vous renommez un `.qmd`, déplacer `_freeze/<nom>` ne suffit pas.** Les fichiers JSON de
   résultats d'exécution contiennent des chemins de figures `<nom>_files/...`, qu'il faut réécrire
   vers le nouveau nom, sinon le HTML généré pointera vers des dossiers de figures inexistants.

Le chunk de setup (packages, chargement des données, `demographic_variables`,
`demographic_variables_not_reported`, `program_colours`) est **dupliqué** entre `report.qmd`,
`report.en.qmd`, `tables.qmd` et `codebook.qmd`. Si vous en modifiez un, modifiez les quatre. (Une
différence connue et volontaire : `tables.qmd` et `codebook.qmd` utilisent `motif_rupture` là où les
fichiers du rapport utilisent `rupture`.)

---

## 4. Ajouter une nouvelle enquête (une nouvelle promo)

C'est la tâche de maintenance principale. Lisez d'abord la section 5 sur les risques : ce pipeline est
plus fragile qu'il n'y paraît, et l'essentiel du travail relève de la vérification, pas du code.

Prévoyez **une journée de travail complète au minimum**, dont la plus grande part sur l'étape 4.3.

### 4.1 — Mettre les données en place

Déposez l'export de la nouvelle cohorte dans `data/Promo 24-25.xlsx` (exactement ce nommage — le code
en dépend), avec les cinq feuilles dans l'ordre décrit au §1. Enregistrez également les PDF des
questionnaires (`Q1 2024-2025 - Trame des questions.pdf`, etc.) : ils seront nécessaires à
l'étape 4.3.

### 4.2 — Ajouter une section à `cleaning.qmd`

Copiez toute la section `# Promo 2023-24` et changez l'année partout. Le bloc fait cinq choses : lire
les feuilles, les nommer, les nettoyer à l'aide du codebook, retirer les colonnes administratives des
feuilles de questionnaire, joindre, et écrire `data/cleaned_promo_24-25.csv`.

**Attention au nom d'objet `promo_2X`.** La section de chaque cohorte nomme sa feuille administrative
d'après la cohorte — `promo_20`, `promo_21`, `promo_22`, `promo_23` — puis se réfère à ce nom à la
main à cinq endroits : `better_names`, `vars_to_remove`, la condition `if (name != "promo_23")`, la
ligne `cleaned_sheets$promo_23 <- NULL`, et le `full_join(..., promo_23 |> select(-source), ...)`.
Quand vous copiez la section, ces cinq occurrences doivent devenir `promo_24`. En oublier une donne
soit une erreur « objet introuvable » (cas heureux), soit une jointure silencieusement fausse (cas
malheureux).

Notez que les *commentaires* de ces chunks disent « promo_20 » dans toutes les sections : ils ont été
copiés-collés sans jamais être mis à jour. Ignorez-les, lisez le code.

### 4.3 — Faire concorder les nouvelles questions avec `data/codebook.csv` (le plus difficile)

Exécutez d'abord le chunk qui écrit `data/codebook_prelimiary_24-25.csv`. Il liste chaque question du
nouveau fichier, indique dans quelle(s) vague(s) elle est apparue, et signale les questions qui
apparaissent plusieurs fois.

Une question peut apparaître deux fois pour deux raisons, qu'il faut distinguer **à la main** :

- Le volontaire était inscrit dans **deux programmes** et a donc vu la même question deux fois au sein
  d'une même vague. Ce sont de vrais doublons.
- La question est une **question de relance rattachée à une autre question**, mais dont la formulation
  se trouve être identique (par ex. « Pour quelles raisons ? »). Ce ne sont *pas* des doublons et
  elles ne doivent pas être fusionnées.

Ce sont les PDF des questionnaires qui permettent de trancher.

Vient ensuite le point important. `data/codebook.csv` fait correspondre chaque **texte de question** à
un `variable_name` court. Le code de nettoyage fait ceci, et rien d'autre :

```r
data |>
  select(any_of(codebook$question)) |>
  rename_with(~ codebook$variable_name[match(.x, codebook$question)], .cols = everything())
```

`any_of()` signifie que **toute question dont la formulation ne correspond pas exactement au codebook
est écartée silencieusement. Aucune erreur, aucun avertissement.** Si Unis-Cité a transformé « Sexe »
en « Sexe : » d'une année à l'autre, cette colonne disparaît purement et simplement de l'analyse, et
le rapport affichera la nouvelle cohorte comme entièrement manquante sur cette variable — ce qui
ressemble à un résultat réel plutôt qu'à un bug.

Pour chaque question de la nouvelle cohorte, tranchez donc entre trois cas :

- **Formulation identique à une ligne existante du codebook** → rien à faire, la correspondance se
  fera automatiquement.
- **Même question, nouvelle formulation** → c'est le cas dangereux. Ajoutez une nouvelle ligne au
  codebook avec le *nouveau* texte de question pointant vers le `variable_name` *existant*, afin que
  les deux années atterrissent dans la même colonne. Ne modifiez pas l'ancienne ligne : les anciennes
  cohortes en ont toujours besoin.
- **Question réellement nouvelle** → ajoutez une ligne avec un nouveau `variable_name`. Elle
  n'apparaîtra pas dans le rapport tant que personne n'aura écrit une section pour elle.

Le codebook est maintenu **à la main dans Google Sheets**, puis réimporté sous
`data/codebook.csv`. Ses colonnes sont `question`, `duplicate_flag`, `sources`, `variable_name`,
`multiple_answers`, `answer_options`. Après réimport, copiez-le également à la racine du dépôt sous
`codebook.csv`, pour que la modification soit versionnée.

### 4.4 — Ajouter la promo aux données combinées

La section `# A common data frame` récupère les fichiers `data/cleaned_promo_\d{2}-\d{2}.csv` : la
nouvelle cohorte est donc prise en compte automatiquement et `promo` est extraite du nom de fichier.
En revanche, la section cartographique contient une **liste écrite en dur** :

```r
promos <- c("20-21", "21-22", "22-23", "23-24")   # cleaning.qmd, ~ligne 1165
```

Ajoutez-y la nouvelle promo, sans quoi elle sera absente de toutes les cartes.

### 4.5 — Vérifier les sections de recodage

Tout ce qui suit `## Rupture variable` dans `cleaning.qmd` recode les chaînes de réponses françaises
brutes en variables d'analyse, en comparant ces chaînes **littéralement**. Par exemple :

```r
satisfaction = factor(satisfaction, levels = c(
  "Pas du tout satisfaisante", "Peu satisfaisante",
  "Assez satisfaisante", "Très satisfaisante"))
```

Si la nouvelle cohorte écrit « Très satisfaisant » (sans *e* final), ou si le fichier administratif
utilise un nouveau code de motif de rupture, ces réponses deviennent `NA` — là encore,
silencieusement.

Parcourez chaque section de recodage (`rupture`, `type_volontaire`, `satisfaction`,
`confiance_en_soi`, `confiance_avenir_personnel`, `comparaison_utile_autres`, `fierte`,
`confiance_avenir`, `individual action`, `zone_residence`, `education`, `sex`, `age`, `refugie`,
`programme_grouped`, `key programs`) et confrontez-la aux valeurs de la nouvelle cohorte. La
vérification la plus rapide est un tableau par variable :

```r
combined_data |> count(promo, satisfaction)   # toute promo avec des NA inattendus = formulation non concordante
```

Les deux variables de programme méritent une attention particulière, et elles échouent de manière
différente :

- `programme_grouped` (les grandes catégories : Aidance, Culture, Autre…) est construite en joignant
  un **`tribble` écrit en dur qui liste exactement chaque nom de programme**. Un nom de programme
  absent de cette table — nouveau, renommé, ou simplement orthographié autrement — produit un `NA`.
- `programme_cle` (les sept programmes clés) est construite avec des **expressions régulières** sur
  `programme_1` (`str_detect(programme_1, regex("Solidarité Aidants", ignore_case = TRUE))`). Elles
  tolèrent mieux les petites variations, mais un programme renommé retombe malgré tout en `NA` et
  disparaît des comparaisons entre programmes.

### 4.6 — Regénérer les données cartographiques

Si la nouvelle cohorte comporte de nouvelles valeurs de `site`, la section cartographique demande de
l'attention :

- Les noms de sites sont nettoyés à la main (`recode(site, "Saint-Etienne" = "Saint-Étienne", ...)`).
- Les sites de **La Réunion sont exclus** — les fonds de carte IGN ne couvrent que la France
  métropolitaine.
- **Plusieurs communes françaises portent le même nom** (il y a une Valence dans la Drôme, en Charente
  et en Tarn-et-Garonne). Les noms ambigus sont résolus à la main dans la table `ambiguous_sites`,
  vérifiés à l'aide de la variable `region`. Si un nouveau nom de site est ambigu et que vous ne
  l'ajoutez pas là, ses volontaires seront comptés dans plusieurs départements à la fois.

Le chunk de vérification qui suit immédiatement `commune_matches` doit renvoyer **zéro ligne**. Les
sites ne correspondant ni à une commune ni à un département sont écartés silencieusement : exécutez
le chunk d'anti-jointure et regardez ce qui en tombe.

Regénérez ensuite `data/map.rds`.

### 4.7 — Écrire les données

Exécutez le chunk `# Write out data` pour rafraîchir `data/cleaned_promo_combined.csv` et
`data/cleaned_promo_combined.RData`. Le rapport lit le fichier `.RData`.

### 4.8 — Mettre à jour le rapport

L'essentiel du rapport boucle sur les promos et intègre la nouvelle cohorte automatiquement. Ce qui ne
le fait **pas** :

- **Les noms de promos écrits en dur** — environ 16 occurrences dans `report.qmd` comme dans
  `report.en.qmd`. Trouvez-les avec `grep -n '23-24' report.en.qmd report.qmd`. On y trouve :
  - les cartes de tendance, qui calculent `` `23-24` - `20-21` `` — décidez si la tendance doit
    désormais aller jusqu'à la nouvelle cohorte ;
  - les sections par promo des analyses sur le vote et sur l'action individuelle
    (`### Promo 2023-24`), chacune avec sa figure, son `promo_filter = "23-24"`, sa légende et sa
    prose. Une nouvelle cohorte demande une nouvelle sous-section copiée sur celles-ci, ainsi que sa
    propre interprétation ;
  - des chiffres dans la prose un peu partout (« de 13 % en 2020-21 à 24 % en 2023-24 ») — écrits à la
    main, ils seront **faux tout en restant plausibles** si vous ne les reprenez pas.
- **Les résumés exécutifs** — chaque chiffre d'`index.qmd` / `index.en.qmd` est écrit en dur.
- La prose de la section sur le vote met chaque cohorte en rapport avec les **élections réelles**
  tombées pendant son année de service. Pour une nouvelle cohorte, ce contexte est à rechercher et à
  rédiger.

Rappel : l'anglais d'abord, puis répercussion en français. Pour les modifications mécaniques sur les
deux fichiers, l'approche établie est un petit script Python fondé sur des remplacements de chaînes
littérales assortis d'assertions sur le nombre d'occurrences, afin qu'un remplacement silencieusement
sans effet ne puisse pas passer inaperçu.

### 4.9 — Générer et vérifier

```bash
quarto render
```

Puis regardez réellement le résultat. Le mode de défaillance de ce pipeline n'est pas le plantage :
c'est un graphique qui s'affiche magnifiquement alors qu'une année entière de données manque
discrètement. Vérifiez en particulier :

- que chaque variable présente des effectifs non manquants plausibles pour la **nouvelle** promo
  (`count(promo, <var>)`) ;
- que les taux de réponse de la section sur l'attrition sont plausibles pour la nouvelle cohorte ;
- que la nouvelle cohorte apparaît sur les cartes, et que l'effectif total de volontaires correspond à
  ce qu'attend Unis-Cité ;
- que les comparaisons entre programmes clés comportent toujours les sept programmes ;
- que les renvois et les figures se résolvent dans le PDF, et pas seulement dans le HTML.

---

## 5. Risques et limites — à lire

**Ce pipeline échoue silencieusement.** C'est la chose la plus importante à savoir à son sujet. Il y a
quatre endroits où une non-concordance produit des données manquantes plutôt qu'une erreur :

1. `select(any_of(codebook$question))` — une question dont la formulation a changé est **écartée sans
   avertissement**.
2. Les sections de recodage comparent littéralement les chaînes de réponses françaises : une option de
   réponse modifiée devient `NA`.
3. La table de correspondance de `programme_grouped` — un nom de programme non listé devient `NA`.
4. Les feuilles de chaque fichier Excel sont identifiées **par position** : un fichier réordonné
   étiquette tout de travers.

Dans ces quatre cas, le rapport se génère quand même, et la nouvelle cohorte donne simplement
l'impression d'avoir beaucoup de données manquantes ou de présenter un motif nouveau et intéressant.
**Ne jamais supposer qu'un rendu réussi signifie des données correctes.** Comparez toujours les
effectifs par promo avant de croire un résultat.

Autres points à garder à l'esprit :

- **Le codebook est maintenu à la main**, dans Google Sheets, en dehors de ce dépôt. C'est la véritable
  interface entre l'enquête et l'analyse, et il n'a aucun test. Traitez ses modifications avec le même
  soin que des modifications de code.
- **Les questions en double sont identifiées à la main.** Les volontaires inscrits dans deux
  programmes répondent deux fois à certaines questions ; certaines questions identiquement formulées
  sont en réalité des relances distinctes. Le repérage automatique surdétecte, et quelqu'un doit lire
  les PDF des questionnaires pour trancher.
- **Les lignes en double sont résolues en gardant la première occurrence.** Lorsqu'un volontaire
  apparaît deux fois dans une vague, `slice(1)` retient la première — y compris là où les deux lignes
  comportent des réponses réellement différentes. C'était une décision pragmatique, prise faute de
  meilleure information.
- **Les cartes ne couvrent que la France métropolitaine.** Les volontaires de La Réunion sont exclus à
  la main, et leurs noms de sites (« Saint-Denis », « Saint-Pierre », « Saint-Benoît ») existent aussi
  en métropole : la liste d'exclusion repose donc sur les noms et a été vérifiée à l'aide de `region`.
  Ajouter une cohorte comportant des sites ultramarins suppose de refaire cette vérification.
- **Les chiffres présents dans la prose du rapport sont écrits en dur.** Les résultats calculés en
  ligne à partir des données se mettent à jour tout seuls ; des phrases comme « 48 % en 2020-21 »,
  non. Après toute modification des données, relisez la prose.
- **La modélisation est délibérément descriptive** : une régression simple (logistique) par
  prédicteur, assortie d'un indicateur de significativité, présentée sous forme de graphiques en forêt
  et de tableaux en annexe. C'est un choix, pas un oubli. Il existe une annexe multivariée, mais le
  corps du texte est univarié de bout en bout. Toute nouveauté devrait suivre le même schéma plutôt
  que d'introduire une autre approche de modélisation.
- **Tout relève de l'association.** Aucune affirmation causale, nulle part. En particulier, les
  différences entre programmes reflètent autant qui s'y inscrit que ce qui s'y passe.
- **`data/` doit rester en dehors de git.** Le dossier contient des données personnelles.

### Conventions de style à conserver

- La prose du rapport nomme explicitement les groupes et le sens de la différence (« les volontaires
  sans le bac déclarent une satisfaction plus faible que… ») plutôt que d'écrire « X prédit Y ».
- Le public est non technique. Les statistiques sont expliquées en langage courant, avec des exemples
  concrets.
- Les sections suivent une trame récurrente : vue d'ensemble → tendance entre promos → prédicteurs →
  différences entre programmes clés. Les nouvelles sections doivent s'y conformer.
- Lorsque vous touchez du code dupliqué entre plusieurs chunks, factorisez-le dans
  `functions/functions.R` plutôt que de rafistoler les copies. Vérifiez une refactorisation en
  regénérant et en comparant les empreintes des figures
  (`md5 -q docs/index_files/figure-html/<fig>.png`).
- Des libellés français apparaissent à l'intérieur des figures anglaises, parce que les valeurs des
  données sont les options de réponse françaises. C'est attendu, ce n'est pas un bug.
- Toute nouvelle page bilingue doit être ajoutée à la table `pairs` de `language-toggle.html`. Ce
  fichier fait **uniquement de la réécriture de liens, jamais de redirection** — une version antérieure
  fondée sur des redirections bouclait lorsque localStorage était indisponible (par ex. en navigation
  privée sous Safari).

---

## 6. Contact

Le rapport a été rédigé par Jan Pfänder (janlukas.pfaender@gmail.com). Sources :
<https://github.com/janpfander/unis-cite>.
