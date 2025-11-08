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
          label = round(estimate, digits = 2)
        )
    })
  
  return(results)
}





