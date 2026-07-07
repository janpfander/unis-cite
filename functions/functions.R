# set general theme for plots
plot_theme <- theme_minimal(base_size = 12) +
  theme(# Bold title
    plot.title = element_text(face = "bold", size = rel(1)),
    # Plain subtitle that is grey
    plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
    # Bold legend titles
    legend.title = element_text(face = "bold", size = 8),
    # Bold axis titles
    axis.title = element_text(face = "bold"), 
    # Bold legend labels
    legend.text = element_text(size = 8)
  )

# round numbers from models
rounded_numbers <- function(x) mutate_if(x, is.numeric, round, 1)

# get model outputs ready for inline text reporting
text_ready <- function(model_output) {
  
  result <- model_output
  
  if ("effect" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      # without the second bit, the normal lm() result would not be selected
      filter(effect == "fixed" | is.na(effect))
  } 
  
  result <- result %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ paste0("= ", sprintf("%.3f", p.value))
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>% 
    mutate(ci = glue::glue("[{conf.low}, {conf.high}]")) 
  
  if ("term" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      mutate(
        term = ifelse(term == "(Intercept)", "intercept", term),
      ) %>% 
      super_split(model, term)
  } 
  
  return(result)
}

# clean data frame results for paired t-test
clean_t_test <- function(t.test) {
  
  t.test %>% 
    tidy() %>% 
    select(-c(method, alternative)) %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ sprintf("p = %.3f", p.value)
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>%
    mutate_all(~ {attributes(.) <- NULL; .})
  
}

# Function for splitting data along several variables (useful for inline reporting)
# taken from here: https://www.tjmahr.com/lists-knitr-secret-weapon/
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}

# Fit one model with all predictors at once (in contrast to run_regression /
# run_program_models, which fit one model per predictor). Keeps one non-NA
# outcome per volunteer and uses complete cases on all predictors.
run_multivariable_model <- function(data, outcome_var, predictors, type = "linear") {

  model_data <- data |>
    arrange(id_jeune, is.na(.data[[outcome_var]])) |>  # NA last
    group_by(id_jeune) |>
    slice(1) |>  # keep first (non-NA if available)
    ungroup() |>
    select(all_of(c(outcome_var, predictors))) |>
    drop_na()

  formula <- reformulate(predictors, response = outcome_var)

  if (type == "logistic") {
    model <- glm(formula, data = model_data, family = "binomial")
    results <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  } else {
    model <- lm(formula, data = model_data)
    results <- broom::tidy(model, conf.int = TRUE)
  }

  results |>
    filter(term != "(Intercept)") |>
    mutate(
      outcome = outcome_var,
      n = nrow(model_data),
      label = round(estimate, digits = 2),
      variable = map_chr(term, function(t) predictors[which(startsWith(t, predictors))[1]]),
      significant = ifelse(p.value <= 0.05, TRUE, FALSE)
    )
}

# Function to run regression and tidy results
run_regression <- function(data, dependent_var, independent_var, return = "data") {
  
  model <- lm(as.formula(paste(dependent_var, "~", independent_var)), data = data) 
  
  if(return == "data") {
    
    data_frame <- model %>%
      tidy(conf.int = TRUE) %>%
      mutate(outcome = {{dependent_var}})  %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ paste0("= ", sprintf("%.3f", p.value))
    )
    ) %>% 
      # all other terms
      rounded_numbers() %>% 
      mutate(
        term = ifelse(term == "(Intercept)", "intercept", term),
        ci = glue::glue("[{conf.low}, {conf.high}]"), 
      ) 
    
    return(data_frame)
  }
  
  if(return == "model") {
    
    return(model)
  }
}

# run models for different programs
run_program_models <- function(
    data,
    outcome_var,
    id_var = id_jeune,
    predictor_sets
) {
  # ---- Setup ----
  outcome_var <- rlang::ensym(outcome_var)
  id_var <- rlang::ensym(id_var)
  
  # ---- Prepare data: keep one non-NA outcome per participant ----
  model_data <- data |>
    arrange(!!id_var, is.na(!!outcome_var)) |>  # NA last
    group_by(!!id_var) |>
    slice(1) |>  # keep first (non-NA if available)
    ungroup()
  
  # ---- Fit one model per predictor ----
  results <- predictor_sets |>
    map_dfr(function(var) {
      df <- model_data |>
        select(!!outcome_var, !!sym(var)) |>
        drop_na()
      
      # Only run if predictor has 2+ unique levels
      if (n_distinct(df[[var]]) < 2) {
        return(NULL)
      }
      
      formula <- as.formula(glue::glue("{rlang::as_name(outcome_var)} ~ `{var}`"))
      
      model <- glm(formula, data = model_data, family = "binomial")
      
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        mutate(
          variable = var,
          outcome = rlang::as_name(outcome_var),
          label = round(estimate, digits = 2),
          significant = ifelse(p.value <= 0.05, TRUE, FALSE)
        )
    })
  
  return(results)
}

# program descriptive plots
plot_faceted_distribution <- function(data, plot_variables, fill_var, program_colour = "Blue") {
  
  fill_var <- rlang::ensym(fill_var)
  
  
  # store level orders (works for factors and non-factors)
  level_orders <- lapply(data[plot_variables], function(x) {
    if (is.factor(x)) levels(x) else unique(x)
  })
  
  plot_data <- data %>%
    mutate(across(all_of(plot_variables), as.character)) %>%
    pivot_longer(cols = all_of(plot_variables),
                 names_to = "variable",
                 values_to = "level") %>%
    drop_na(level) %>%
    drop_na(!!fill_var) %>%
    group_by(variable, level, !!fill_var) %>%
    summarize(n = n_distinct(id_jeune), .groups = "drop") %>%
    group_by(variable, !!fill_var) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
  
  # restore original factor order
  plot_data <- plot_data %>%
    group_by(variable) %>%
    mutate(level = factor(level, levels = level_orders[[unique(variable)]])) %>%
    ungroup()
  
  ymax <- max(plot_data$pct, na.rm = TRUE) * 1.2
  
  # get levels
  fill_levels <- levels(plot_data[[rlang::as_name(fill_var)]])
  
  ggplot(plot_data,
         aes(x = level, y = pct, fill = factor(!!fill_var))) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3
    ) +
    scale_fill_manual(
      values = c(
        "#D8D2C2",
        program_colour
      )
    ) +
    facet_wrap(~variable, scales = "free_x") +
    labs(
      x = NULL,
      y = "Share",
      fill = "Programme"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      breaks = seq(0, 100, 20)
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "top"
    )
}





# Plot within-person change of a variable between two survey waves, for one promo.
# Returns a list with two plots: an alluvial plot and a summary of changes in percent
# (relative to all volunteers who answered at both waves).
plot_within_change <- function(data, variable, promo_filter,
                               waves = c("q1", "q3"),
                               answer_levels,
                               fill_label,
                               change_colours = c(
                                 "vers Pas vraiment" = "#E1AF00",
                                 "vers Non" = "#F21A00",
                                 "vers Oui" = "#3B9AB2"
                               )) {
  # wide format data, one row per volunteer who answered at both waves
  wide_data <- data |>
    filter(promo == promo_filter) |>
    drop_na(!!sym(variable)) |>
    select(id_jeune, source, !!sym(variable)) |>
    pivot_wider(names_from = source, values_from = !!sym(variable)) |>
    filter(!is.na(.data[[waves[1]]]) & !is.na(.data[[waves[2]]]))

  # long format for the alluvial plot
  long_data <- wide_data |>
    pivot_longer(cols = all_of(waves), names_to = "time", values_to = "answer") |>
    mutate(answer = factor(answer, levels = answer_levels))

  alluvial_plot <- ggplot(long_data,
                          aes(x = time, stratum = answer, alluvium = id_jeune,
                              fill = answer, label = answer)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback", alpha = 0.7) +
    geom_stratum() +
    scale_fill_manual(values = wes_palette("Zissou1", 5, type = "discrete")[c(3, 5, 1)]) +
    scale_x_discrete(expand = c(0.1, 0.1)) +
    labs(
      x = "Temps",
      y = "Nombre d'individus",
      fill = fill_label
    ) +
    plot_theme

  # summarize changes ("vers <answer at second wave>" for those who changed)
  change_summary <- wide_data |>
    mutate(change = ifelse(
      .data[[waves[1]]] == .data[[waves[2]]],
      "Pas de changement",
      paste("vers", .data[[waves[2]]])
    )) |>
    count(change) |>
    mutate(prop = n / sum(n))

  change_plot <- ggplot(change_summary, aes(x = change, y = prop, fill = change)) +
    geom_col() +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)),
              vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = change_colours) +
    guides(fill = "none") +
    labs(
      subtitle = paste0("N = ", nrow(wide_data),
                        " volontaires ayant répondu aux deux questionnaires"),
      x = "Changé",
      y = "Proportion",
      fill = NULL
    ) +
    plot_theme

  list(alluvial = alluvial_plot, change = change_plot)
}

# Plot within-person change in reported voting behavior between q1 and q2, for one
# promo. Same logic as plot_within_change, but with the vote-specific recoding:
# the long answer option is shortened to "Pas sur les listes", and volunteers who
# changed *towards* "Pas sur les listes" (a small minority) are not shown in the
# change plot.
plot_vote_change <- function(data, promo_filter) {

  vote_data <- data |>
    filter(promo == promo_filter) |>
    drop_na(vote_derniers_elections) |>
    mutate(vote_derniers_elections = case_when(
      vote_derniers_elections == "Vous n'aviez pas l'âge de voter ou vous n'étiez pas inscrit.e sur les listes électorales" ~ "Pas sur les listes",
      TRUE ~ vote_derniers_elections
    )) |>
    select(id_jeune, source, vote_derniers_elections) |>
    pivot_wider(names_from = source, values_from = vote_derniers_elections, names_prefix = "vote_") |>
    filter(!is.na(vote_q1) & !is.na(vote_q2)) |>
    mutate(across(starts_with("vote_"), ~ factor(., levels = c("Pas sur les listes", "Non", "Oui"))))

  # long format for the alluvial plot
  vote_long <- vote_data |>
    pivot_longer(cols = starts_with("vote_q"), names_to = "time", values_to = "vote") |>
    mutate(time = recode(time, "vote_q1" = "q1", "vote_q2" = "q2"))

  pal <- wes_palette("Zissou1", 5, type = "discrete")[c(3, 5, 1)]

  alluvial_plot <- ggplot(vote_long,
                          aes(x = time, stratum = vote, alluvium = id_jeune,
                              fill = vote, label = vote)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback", alpha = 0.7) +
    geom_stratum() +
    scale_fill_manual(values = pal) +
    scale_x_discrete(expand = c(0.1, 0.1)) +
    labs(
      x = "Temps",
      y = "Nombre d'individus",
      fill = "Vote"
    ) +
    plot_theme

  vote_change_summary <- vote_data |>
    mutate(change = case_when(
      vote_q1 == vote_q2 ~ "Pas de changement",
      vote_q1 == "Oui" & vote_q2 == "Non" ~ "Changé vers Non",
      vote_q1 == "Non" & vote_q2 == "Oui" ~ "Changé vers Oui",
      vote_q1 == "Pas sur les listes" & vote_q2 == "Non" ~ "Changé vers Non",
      vote_q1 == "Pas sur les listes" & vote_q2 == "Oui" ~ "Changé vers Oui",
      TRUE ~ NA_character_
    )) |>
    drop_na(change) |>
    count(change) |>
    mutate(prop = n / sum(n))

  change_plot <- ggplot(vote_change_summary, aes(x = change, y = prop, fill = change)) +
    geom_col() +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)),
              vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = c(
      "Changé vers Non" = "#F21A00",
      "Changé vers Oui" = "#3B9AB2"
    )) +
    guides(fill = "none") +
    labs(
      subtitle = paste0("N = ", nrow(vote_data),
                        " volontaires ayant répondu aux deux questionnaires"),
      x = NULL,
      y = "Proportion",
      fill = NULL
    ) +
    plot_theme

  list(alluvial = alluvial_plot, change = change_plot)
}
