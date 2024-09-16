# This document is meant to be loaded for its multipurpose functions.
library(cdrs)
library(tidyverse)
library(survey)
library(dplyr)
library(rlang)
library(ggplot2)
library(ggforce)

# some functions from "https://github.com/ktomari/drs/blob/main/Crosstabs/crosstab_functions.R"


  
# Percentage of the circle to outline
create_circle_with_arc <- function(percentage, arc_color) {
  # Convert the percentage to an end angle in radians for geom_arc(), counter-clockwise orientation
  # 0 radians is the rightmost point of the circle, subtract from -pi/2 to start at the top
  start_angle <- pi *2
  end_angle <- start_angle - 2 * pi * percentage / 100
  
  # Create a data frame with the circle information
  circle_data <- data.frame(
    x0 = 0,
    y0 = 0,
    r = 1
  )
  
  # Base plot: circle outline
  p <- ggplot() +
    geom_circle(data = circle_data, aes(x0 = x0, y0 = y0, r = r), fill = "grey98", color = "grey98", linetype = 1, size = 1) +
    coord_fixed() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"))
  p
  # Add the arc (partial circle outline)
  p <- p + geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = end_angle, end = start_angle), color = arc_color, size = 2) +
    xlim(c(-1.2, 1.2)) +
    ylim(c(-1.2, 1.2))
  percentage_text <- paste0(percentage, "%")
  p <- p + geom_text(aes(x = 0, y = 0, label = percentage_text), size = 25)
  
  # Print the plot
  print(p)
}


fn_var_extract <- function(stem_){
  # get id
  id_ <- str_extract(stem_, "(?<=Q)\\d{1,2}[[:alpha:]]*")
  
  # get all qids
  cb %>%
    filter(id == id_) %>%
    pull(qid) %>%
    unique()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This next set of functions help us derive the 
# first-level of chi-squared tests.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# `fn_subset` takes the two variables, and makes a data subset, making sure to 
# removing missing levels for the two variables, as well as the survey weights 
# and survey strata (Zone).

#' Create data subset.
#' @param qid_ character(1), eg "Q3_0". This is the quasi-"dependent" variable.
#' @param grp_ character(1), eg "Zone". This is the quasi-"independent" variable.
fn_subset <- function(res_, grp_, custom_data = NA){
  # RETURN
  # Subset the data.
  if(length(custom_data) == 1){
    data %>%
      select(
        all_of(res_),
        all_of(grp_),
        WTFINAL,
        # Zone may be redundant with grp_,
        # but `select` won't duplicate it, and will ignore it.
        Zone
      ) %>%
      # Remove NA values.
      filter(!is.na(WTFINAL)) %>%
      filter(!is.na(!!sym(res_))) %>%
      filter(!is.na(!!sym(grp_))) %>%
      mutate(!!sym(res_) := fct_drop(as_factor(!!sym(res_)))) %>%
      mutate(!!sym(grp_) := fct_drop(as_factor(!!sym(grp_)))) %>%
      mutate(tmp_id = 1:n())
  } else {
    custom_data %>%
      select(
        all_of(res_),
        all_of(grp_),
        WTFINAL,
        # Zone may be redundant with grp_,
        # but `select` won't duplicate it, and will ignore it.
        Zone
      ) %>%
      # Remove NA values.
      filter(!is.na(WTFINAL)) %>%
      filter(!is.na(!!sym(res_))) %>%
      filter(!is.na(!!sym(grp_))) %>%
      mutate(!!sym(res_) := fct_drop(as_factor(!!sym(res_)))) %>%
      mutate(!!sym(grp_) := fct_drop(as_factor(!!sym(grp_)))) %>%
      mutate(tmp_id = 1:n())
  }
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# `fn_design`creates a survey design object, using the result of `fn_subset`.

#' Create complex survey design object.
#' @param sub_ tibble. Subset of data.
fn_design <- function(sub_){
  # Complex Survey Design Object
  design_ <- svydesign(
    ids = ~1,
    strata = ~Zone,
    fpc = NULL,
    data = sub_,
    weights = ~WTFINAL
  )
  
  # Return
  design_
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# `fn_chisq` uses the outputs of the previous two functions to run a 
# chi-squared test using the default settings of `svychisq` which produces an 
# F-score using a Rao-Scott second-roder correction. "The p-values are computed 
# with a Satterthwaite approximation to the distribution and with denominator 
# degrees of freedom as recommended by Thomas and Rao (1990)."

#' Primary iteration chi-squared test.
#' @param res_ character(1), quasi-dependent/response variable
#' @param grp_ character(1), quasi-independent/grouping variable.
fn_chisq1 <- function(res_, grp_, chisq_as_table = T, return_table = F, custom_data = NA){
  
  # subset data
  if(length(custom_data) == 1){
    sub_ <- fn_subset(
      res_ = res_,
      grp_ = grp_
    )
  } else {
    sub_ <- fn_subset(
      res_ = res_,
      grp_ = grp_,
      custom_data = custom_data
    )
  }
  
  
  # create complex survey design object
  design_ <- fn_design(sub_)
  
  # run chi-squared test
  chisq_ <- svychisq(
    formula = as.formula(paste0(
      "~", 
      grp_, 
      " + ", 
      res_ 
    )),
    design = design_
  )
  
  if(chisq_as_table){
    chisq_ <- tibble(
      response = res_,
      grouping = grp_,
      # F
      statistic = chisq_$statistic,
      # ndf
      ndf = chisq_$parameter[1],
      # ddf
      ddf = chisq_$parameter[2],
      # pvalue
      pvalue = chisq_$p.value,
      # specific chi-squared method
      method = chisq_$method
    )
  }
  
  # Return
  if(return_table){
    list(
      sub = sub_,
      table = svytable(
        as.formula(
          paste0("~", 
                 grp_, 
                 " + ", 
                 res_ )), 
        design = design_),
      chisq = chisq_
    )
  } else {
    list(
      sub = sub_,
      chisq = chisq_
    )
  } # end if statement
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


####

calculate_svymean <- function(data, filter_col, operation, filter_value, question) {
  # Filter the data
  filter_expr <- rlang::sym(filter_col)
  if (operation == "!=") {
    filter_expr <- rlang::expr(!!filter_expr != !!filter_value)
  } else if (operation == "%in%") {
    filter_expr <- rlang::expr(!!filter_expr %in% !!filter_value)
  } else {
    filter_expr <- rlang::expr(!!filter_expr == !!filter_value)
  }
  
  # Filter the data
  subdata <- data %>% filter(!!filter_expr)
  
  # Create survey design object with Complex Design Response Surveys package (cdrs)
  subdata_obj <- cdrs::cdrs_design(subdata, set_fpc = TRUE)
  
  # Calculate and return the survey mean for the specified question
  svymean_result <- survey::svymean(
    x = as.formula(paste0("~", question)),
    design = subdata_obj,
    na.rm = TRUE
  )
  print(paste(filter_col,operation,filter_value))
  return(svymean_result)
}

calculate_proportions <- function(data, q, group) {
  crosstab_result <- data.frame(cdrs_crosstab(data, c(q, group)) %>%
    group_by(!!sym(group)) %>%
    mutate(Prop = Freq / sum(Freq)) %>%
    ungroup()) %>%
    filter(!!sym(q) == "Yes")
  
  return(crosstab_result)
}

svymean_yes_reponses <- function(data, q, q_range){
  data_obj <- cdrs::cdrs_design(data, set_fpc = TRUE)
  
  response <- data.frame(survey::svymean(
  x =  as.formula(paste0("~",paste0(q,"_", q_range, collapse = "+"))),
  design = data_obj,
  na.rm = TRUE
))

  response <- response[grep("Yes", rownames(response)),]
  response[order(response$mean, decreasing = T),]
}

plot_demos <- function(data, demo, color_bar){
  ggplot(data %>% 
         filter(!is.na(!!sym(demo) )) %>%
         count(!!sym(demo)) %>%
         mutate(percent = round(n / sum(n), digits = 2))
       , aes(x = "", y = percent, fill = !!sym(demo))) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.text = element_text(size = 17)) +
  scale_fill_manual(values = color_bar) + 
  geom_text(aes(label = ifelse(percent >= 0.06, scales::percent(percent), "")),
            position = position_stack(vjust = 0.5),size=7) +
  theme(legend.position = "right") +
  labs(fill = "")
}

