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

# function to calculate models
calculate_models <- function(predictor, return_as = "tidy") {  
  # Define model formulas dynamically  
  formula_lmer <- as.formula(paste("trust ~", predictor, "+ (1 +", predictor, "| id)"))  
  formula_lmer_within <- as.formula(paste("trust ~", predictor, "+ (1 +", predictor, "| id) + (1 +", predictor, "| discipline)"))  
  formula_lm <- as.formula(paste("trust ~", predictor))  
  
  # Fit models  
  model_across <- lmer(formula_lmer, data = data_long %>% filter(discipline != "general"))  
  model_within <- lmer(formula_lmer_within, data = data_long %>% filter(discipline != "general"))  
  model_general <- lm(formula_lm, data = data_long %>% filter(discipline == "general"))  
  
  # Ensure return_as is valid  
  if (!return_as %in% c("tidy", "raw")) {  
    stop('Invalid value for return_as. Use "tidy" or "raw".')  
  }  
  
  # Return raw models if specified  
  if (return_as == "raw") {  
    return(list(  
      across_disciplines = model_across,  
      within_disciplines = model_within,  
      science_in_general_only = model_general  
    ))  
  }  
  
  # Otherwise, return tidy results  
  results <- bind_rows(  
    tidy(model_across, conf.int = TRUE) %>% mutate(model = "across_disciplines"),  
    tidy(model_within, conf.int = TRUE) %>% mutate(model = "within_disciplines"),  
    tidy(model_general, conf.int = TRUE) %>% mutate(model = "science_in_general_only")  
  ) %>% rounded_numbers()  
  
  return(results)  
}

plot_between <- function(data, x_var) {
  
  # Group data by discipline and calculate the mean for the selected x variable and trust
  plot_data_between <- data |> 
    group_by(discipline) %>% 
    select(trust, all_of(x_var)) %>% 
    summarise(across(c(trust, all_of(x_var)), ~mean(.x, na.rm = TRUE), 
                     .names = "mean_{.col}")
    )
  
  # Generate plot with the specified x variable
  plot <- ggplot(plot_data_between, aes_string(x = paste0("mean_", x_var), y = "mean_trust")) + 
    geom_point() +
    geom_label_repel(aes(label = discipline), size = 3, nudge_y = -0.02) +
    labs(x = paste(x_var, "[Scale from 1 to 5]"), y = "Trust [Scale from 1 to 5]") +
    plot_theme +
  # Set axis limits and labels
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) 
  return(plot)
}


plot_within <- function(data, x_var) {
  
  # Aggregate data: Count participants per (consensus, trust) pair
  count_data <- data |> 
    filter(discipline != "general") |> 
    group_by(discipline, {{x_var}}, trust) |> 
    summarize(participant_count = n_distinct(id))
  
  # Plot with precomputed participant count
  plot <- ggplot(count_data, aes(x = {{x_var}}, y = trust)) + 
    # Tile fill reflects participant count
    geom_tile(aes(fill = participant_count), alpha = 0.9) +
    # Jittered points for individual observations
    geom_jitter(data = data |> filter(discipline != "general"), 
                aes(x = {{x_var}}, y = trust), 
                width = 0.3, height = 0.2, alpha = 0.2, color = "black", 
                size = 0.5) +
    # Single color gradient
    scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Participant Count") +
    # Regression line
    geom_smooth(data = data |> filter(discipline != "general"), 
                aes(x = {{x_var}}, y = trust), method = "lm", se = FALSE) +
    # Set axis limits and labels
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    # Labels & Theme
    labs(
      x = str_to_title(as.character(substitute(x_var))),  # Convert x_var to uppercase
      y = "Trust [Scale from 1 to 5]") +
    plot_theme +
    facet_wrap(~discipline) +
    theme(legend.position = c(0.9, 0.2), 
          legend.direction = "horizontal",
          legend.key.width = unit(0.4, "cm")
    ) +
    guides(fill = guide_colorbar(title.position = "top"))
  
  return(plot)
  
}

