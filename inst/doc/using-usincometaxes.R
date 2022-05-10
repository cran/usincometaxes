## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.align = 'center',
  fig.path = 'webimg/',
  fig.width = 8,
  fig.height = 5,
  dpi = 72,
  dev = 'png'
)

## ----setup, include = FALSE---------------------------------------------------
library(usincometaxes)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)

## ----import_data--------------------------------------------------------------
data(taxpayer_finances)

taxpayer_finances %>%
  head() %>%
  kable()

## ----calcualte_survey_taxes---------------------------------------------------
family_taxes <- taxsim_calculate_taxes(
  .data = taxpayer_finances,
  return_all_information = FALSE
)

family_taxes %>%
  head() %>%
  kable()

## ----join_tax_data------------------------------------------------------------
income_and_taxes <- taxpayer_finances %>%
  left_join(family_taxes, by = 'taxsimid')

income_and_taxes %>%
  head() %>%
  kable()

## ----plot_family_taxes, fig.height = 7, fig.width = 9-------------------------
# custom theme for all plots in the vignette
plt_theme <- function() {
  
    theme_minimal() +
    theme(
      legend.text = element_text(size = 11),
      axis.text = element_text(size = 10),
      axis.title=element_text(size=11,face="bold"),
      strip.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = 'bottom'
    )
}
# color palettes for number of children
dep_color_palette <- rev(c('#4B0055','#353E7C','#007094','#009B95','#00BE7D','#96D84B'))

income_and_taxes %>%
  mutate(
    tax_unit_income = pwages + swages,
    num_dependents_eitc = factor(depx, levels = as.character(0:5)),
    filing_status = tools::toTitleCase(mstat)
  ) %>%
  ggplot(aes(tax_unit_income, fiitax, color = num_dependents_eitc)) +
    geom_point(alpha = .5) +
    scale_x_continuous(labels = scales::label_dollar(scale = .001, suffix = "K"), limits = c(0, 200000)) +
    scale_y_continuous(labels = scales::label_dollar(scale = .001, suffix = "K"), limits = c(-10000, 50000)) +
    scale_color_discrete(type = dep_color_palette) +
    facet_grid(rows = vars(mstat), cols = vars(year)) +
    labs(
      title = "Federal Income Taxes by Filing Status, Year, and Number of Children",
      x = "\nHousehold Wages",
      y = "Federal Income Taxes"
    ) +
    plt_theme() +
    guides(color = guide_legend(title = "Number of Childern 18 or Younger", title.position = "top", byrow = TRUE))

## -----------------------------------------------------------------------------
# calculate taxes from 0 to 200,000 in wages
wage_linespace <- seq(0, 200000, 100)

n_kids <- 4

base_family_income <- data.frame(
  year = 2020,
  mstat = 'married, jointly',
  state = 'NC',
  page = 40,
  sage = 40,
  depx = n_kids,
  age1 = n_kids,
  age2 = n_kids,
  age3 = n_kids,
  pwages = wage_linespace,
  swages = 0
)

# create an additional data set with no dependents and add it to the original
family_income <- base_family_income %>%
  bind_rows(
    # make all numeber of dependent columns 0
    base_family_income %>%
      mutate(across(c(depx, age1, age2, age3), ~0))
  ) %>%
  # add unique ID to each row
  mutate(taxsimid = row_number()) %>%
  select(taxsimid, everything())

family_income %>%
  head() %>%
  kable()

## -----------------------------------------------------------------------------
family_income_taxes <- taxsim_calculate_taxes(
  .data = family_income,
  return_all_information = TRUE
)

family_income_taxes %>%
  head() %>%
  kable()

## -----------------------------------------------------------------------------
family_income <- family_income %>%
  left_join(family_income_taxes, by = 'taxsimid')

## -----------------------------------------------------------------------------
family_income_long <- family_income %>%
  select(pwages, depx, fiitax, siitax) %>%
  pivot_longer(cols = c('fiitax', 'siitax'), 
               names_to = 'jurisdiction', values_to = 'taxes_paid') %>%
  mutate(
    jurisdiction = recode(jurisdiction, 'fiitax' = 'Federal Income Taxes', 'siitax' = 'NC State Income Taxes'),
    num_dependents_eitc = factor(depx, levels = as.character(0:5)),
    post_tax_wages = pwages - taxes_paid
  )
# primary_wages, taxes_paid, color = as.character(num_dependents_eitc)
taxes_line_plot <- function(.data, x_var, y_var, color_var) {
  ggplot(.data, aes({{x_var}}, {{y_var}}, color = {{color_var}})) +
    geom_line(size = 1, alpha = .8) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(labels = scales::label_dollar(scale = .001, suffix = "K")) +
    scale_y_continuous(labels = scales::label_dollar(scale = .001, suffix = "K")) +
    scale_color_brewer(type = 'seq', palette = 'Set2')  +
    plt_theme()
  
}
taxes_line_plot(family_income_long, pwages, taxes_paid, num_dependents_eitc) +
  facet_wrap(vars(jurisdiction)) +
  labs(
    title = "Relationship Between Wages and Income Taxes Paid",
    subtitle = "Taxpayer is married, filing jointly, in 2020",
    x = "\nPre-Tax Household Wages",
    y = "Federal Income Taxes",
    color = 'Number of Children 18 or Younger:'
  )

## -----------------------------------------------------------------------------
taxes_line_plot(family_income_long, pwages, post_tax_wages, num_dependents_eitc) +
  facet_wrap(vars(jurisdiction)) +
  labs(
    title = "Relationship Between Pre and Post-Tax Wages",
    subtitle = "Taxpayer is married, filing jointly, in 2020",
    x = "\nPre-Tax Household Wages",
    y = "Post-Tax Hosuehold Wages",
    color = 'Number of Children 18 or Younger:'
  )

## -----------------------------------------------------------------------------
tax_items_mapping <- c(
  v25_eitc = 'Earned Income Tax Credit',
  child_tax_credit = 'Child Tax Credit'
)

family_income %>%
  filter(depx == 4) %>%
  mutate(child_tax_credit = v22_child_tax_credit_adjusted + v23_child_tax_credit_refundable) %>%
  select(pwages, fiitax, v25_eitc, child_tax_credit) %>%
  pivot_longer(cols = names(tax_items_mapping), names_to = 'tax_item', values_to = 'amount') %>%
  mutate(tax_item = recode(tax_item, !!!tax_items_mapping)) %>%
  taxes_line_plot(pwages, amount, tax_item) +
  labs(
    title = "Relationship Between Wages and Credits",
    subtitle = "Taxpayer is married, filing jointly, in 2020 and has four children under 19",
    x = "\nPre-Tax Wages",
    y = "Credit Amount",
    color = NULL
  )

