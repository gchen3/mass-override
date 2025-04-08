library(tidyverse)
library(foreign)
library(readxl)
library(here)
library(gt)
library(ggplot2)

file_name <- "LongTermDebt.xlsx"
debt_long <- read_excel(
  path = here::here("data", file_name),
  col_names = TRUE)

names(debt_long)

debt_long <- debt_long %>%
  rename(
    municipal = "Municipality",
    debt = "Total Outstanding Debt (Schedule A Part 10)",
    year = "Fiscal Year",
    debt_per = "Outstanding Debt Per Capita",
    debt_value = "Outstanding Debt as a % of EQV",
    debt_ser_budget = "Debt Service as a % of Budget"
  ) %>%
  select(municipal, year, debt, debt_per, debt_value, debt_ser_budget) %>%
  mutate(
    debt = as.numeric(debt),
    debt_per = as.numeric(debt_per),
    debt_ser_budget = as.numeric(debt_ser_budget),
    debt_value = as.numeric(debt_value)
  )

# Compute Acton's debt and group average
debt_comparison <- debt_long %>%
  group_by(year) %>%
  summarise(
    Acton_Debt = sum(debt_per[municipal == "Acton"], na.rm = TRUE),
    MA_Avg_Debt = mean(debt_per, na.rm = TRUE), 
    Acton_Debt_value = sum(debt_value[municipal == "Acton"], na.rm = TRUE),
    MA_Avg_Debt_value = mean(debt_value, na.rm = TRUE),
    Acton_Debt_ser_budget = sum(debt_ser_budget[municipal == "Acton"], na.rm = TRUE),
    MA_Avg_Debt_ser_budget = mean(debt_ser_budget, na.rm = TRUE)
  ) %>%
  ungroup()

# Display the table
debt_comparison %>%
  gt() %>%
  tab_header(
    title = "Debt Comparison: Acton vs MA Average"
  )

debt_long <- read_excel(
  path = here::here("data", file_name),
  col_names = TRUE)

select_town <- c("Acton", "Duxbury", "Cadime", "Boxford", "Billerica", "Seekonk", "Hudson")

pop_town <- debt_long |> 
  filter(Municipality %in% select_town) |>
  select(Municipality, "Fiscal Year", Population) |>
  filter(`Fiscal Year` == 2024) |>
  arrange(Municipality)

pop_town
