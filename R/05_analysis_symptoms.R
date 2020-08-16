# Clear workspace --------------------------------------------------------------
rm(list = ls())


# Load libraries ---------------------------------------------------------------
library("tidyverse")


# Define functions -------------------------------------------------------------
source(file = "R/99_func.R")


# Load data --------------------------------------------------------------------
df_patient <-
  read_csv(file = "data/_augmented/final_patient_data_df_augm.csv",
           col_types = cols())


# Plot 1: Correlation heatmap of categorical symptoms --------------------------
corr_matrix_df <-
  df_patient %>%

  # We select only the categorical symptom columns
  select(chills:discharge) %>%

  # Calculate the correlation matrix
  cor() %>%

  # Transform the results into a tibble with the row names as an "id" column
  as_tibble(rownames = "symptom1") %>%

  # Tidy the data by having all correlation values
  # between two symptoms in the 'value' column
  pivot_longer(cols = -symptom1, names_to = 'symptom2') %>%

  # Round correlation coefficient values to 2 decimals
  mutate(value = round(value, digits = 1))

p_corr_heatmap <-
  ggplot(data = corr_matrix_df,
         mapping = aes(x = symptom1, y = symptom2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(symptom2, symptom1, label = value), color = "black", size = 2) +
  labs(title = "Correlation heatmap of symptoms set",
       x = "Symptom 1", y = "Symptom 2")

ggsave(plot = p_corr_heatmap,
       path = "results",
       filename = "05_symptoms_corr_heatmap.png",
       width = 8,
       height = 8)


# Plot 2: Symptom prevalence grouped by gender ---------------------------------
df_patient_p_2 <-
  df_patient %>%
    pivot_longer(chills:discharge,
                 names_to = 'symptoms',
                 values_to = 'symptom_presence') %>%
    filter(symptom_presence == 1) %>%
    filter(is.na(gender) == FALSE) %>%
    group_by(gender, symptoms) %>%
    summarize(n = n()) %>%
    mutate(prop = n / max(cumsum(n))) %>%
    filter(n > 10)

p_facet_symptoms_gender <-
  ggplot(data = df_patient_p_2,
         aes(y = reorder(symptoms, prop), x = prop)) +
    geom_bar(aes(fill = prop), stat = 'identity') +
    facet_wrap(~gender) +
    labs(title = "Most common symptom prevalence, grouped by gender",
         subtitle = "As of 29.02.2020",
         x = "Proportion of males/females which manifested the symptom",
         y = "Symptom")

ggsave(plot = p_facet_symptoms_gender,
       path = "results",
       filename = "05_symptoms_per_gender.png",
       width = 6,
       height = 5)


# Plot 3: Symptom prevalence grouped by country --------------------------------
df_patient_p_3 <-
  df_patient %>%
    pivot_longer(chills:discharge,
                 names_to = 'symptoms',
                 values_to = 'symptom_presence') %>%
    filter(symptom_presence == 1) %>%
    filter(is.na(country) == FALSE) %>%
    group_by(country, symptoms) %>%
    summarize(n = n()) %>%
    mutate(prop = n / max(cumsum(n))) %>%
    filter(n > 12)

p_facet_symptoms_country <-
  ggplot(data = df_patient_p_3,
         aes(y = reorder(symptoms, prop), x = prop)) +
    geom_bar(aes(fill = prop), stat = 'identity') +
    facet_wrap(~country, ncol = 5) +
    labs(title = "Most common symptom prevalence, grouped by country",
         subtitle = "As of 29.02.2020",
         x = "Proportion of infected people which manifested the symptom",
         y = "Symptom")

ggsave(plot = p_facet_symptoms_country,
       path = "results",
       filename = "05_symptoms_per_country.png",
       width = 9,
       height = 5)


# Plot 4: Facet grid of symptom presence for dead and/or recovered patients ----
df_patient_p_4 <-
  df_patient %>%
    pivot_longer(chills:discharge,
                 names_to = 'symptoms',
                 values_to = 'symptom_presence') %>%
    filter(symptom_presence == 1) %>%
    filter(is.na(is_dead) == FALSE) %>%
    filter(is.na(is_recovered) == FALSE) %>%
    select(symptoms, contains('is_')) %>%
    mutate(is_dead = if_else(is_dead == 1,
                             true = 'Dead_True',
                             false = 'Dead_False')) %>%
    mutate(is_recovered = if_else(is_recovered == 1,
                                  true = 'Recovered_True',
                                  false = 'Recovered_False')) %>%
    group_by(is_dead, is_recovered, symptoms) %>%
    summarize(n = n()) %>%
    mutate(prop = n / max(cumsum(n))) %>%
    filter(prop > 0.01)

p_facet_symptoms_dead_or_recovered <-
  ggplot(data = df_patient_p_4,
         aes(y = reorder(symptoms, prop), x = prop)) +
    geom_bar(aes(fill = prop), stat = 'identity') +
    facet_grid(is_recovered ~ is_dead) +
    labs(title = "Most common symptoms for dead and/or recovered patients",
         subtitle = "As of 29.02.2020",
         x = "Proportion of infected people which manifested the symptom",
         y = "Symptom")

ggsave(plot = p_facet_symptoms_dead_or_recovered,
       path = "results",
       filename = "05_symptoms_dead_or_recovered.png",
       width = 8,
       height = 7)


# Plot 5: Facet plot of the symptom comorbidity --------------------------------
corr_matrix_df %>%
  filter(symptom1 != symptom2) %>%
  filter(value > 0.1) %>%
  arrange(symptom1, desc(value)) %>%
  group_by(symptom1) %>%
  ggplot(aes(y = fct_rev(symptom2), x = value)) +
    geom_bar(aes(fill = value), stat = 'identity') +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         name = 'Correlation\ncoefficient') +
    facet_wrap(~symptom1, ncol = 11) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
    labs(title = "Facet plot of the symptom comorbidity (where r > 0.1)",
         subtitle = "As of 29.02.2020",
         x = "Correlation coefficient between the two symptoms (r)",
         y = "Symptom") +
    ggsave(path = "results",
           filename = "05_symptoms_comorbidity.png",
           width = 16,
           height = 7)
