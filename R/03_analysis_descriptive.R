# Clear workspace --------------------------------------------------------------
rm(list = ls())


# Load libraries ---------------------------------------------------------------
library("tidyverse")
library("leaflet")
library("leaflet.extras")
library("ggpubr")
library("broom")
library("purrr")
library("rpart")
library("rpart.plot")
library("factoextra")
library("caret")
library("gridExtra")

# Define functions -------------------------------------------------------------
source(file = "R/99_func.R")


# Load data --------------------------------------------------------------------
df_patient <-
  read_csv(file = "data/_augmented/final_patient_data_df_augm.csv",
           col_types = cols())

df_ts <-
  read_csv(file = "data/_augmented/final_ts_world_df_augm.csv",
           col_types = cols())


# Basic descriptive and visualization: Patient data ----------------------------
# Plot 1: Distribution of age group by gender ----------------------------------
df_patient %>%
  group_by(age_group, gender) %>%
  tally() %>%
  collect() %>%
  drop_na(gender, age_group) %>%
  arrange(desc(age_group)) %>%
  ggplot() +
  geom_col(aes(x = age_group, y = n, fill = gender)) +
  labs(title = "Distribution of age group by gender",
       subtitle= "COVID-19 affected", x = "Age group", y = "Count") +
  ggsave(path = "results",
         filename = "03_distribution_age_group_gender.png",
         width = 6,
         height = 5)


# Plot 2: Smoothing of time_2_admin ~ age, grouped by gender and dead ----------
df_patient %>%
  mutate(time2admis = as.integer(date_admission_hospital - date_onset)) %>%
  select(gender, age, time2admis, is_dead, contact_with_Wuhan) %>%
  drop_na() %>%
  ggplot() +
  geom_point(aes(age, time2admis, color=gender)) +
  geom_smooth(aes(age, time2admis)) +
  facet_grid(contact_with_Wuhan~.,
             labeller = label_both, scales = "free") +
  ylim(0,30) +
  labs(title = "From onset to hospital admission",
       subtitle= "COVID-19 affected", x = "Age",
       y = "Day(s)") +
  ggsave(path = "results",
         filename = "03_onset_to_admission.png",
         plot = last_plot(),
         width = 6,
         height = 5)


# Plot 3: Boxplot of age range ~ is_dead, contact_with_wuhan and dyspnea -------
df_patient %>%
  select(gender, age, dyspnea, is_dead, contact_with_Wuhan) %>%
  drop_na(gender, age, dyspnea, is_dead, contact_with_Wuhan) %>%
  ggplot() +
  geom_boxplot(aes(as.factor(dyspnea),age, fill=gender)) +
  facet_grid(contact_with_Wuhan~is_dead,
             labeller = label_both, scales = "free") +
  labs(title = "Age distribution by symptoms, death and contact with wuhan",
       subtitle= "COVID-19 affected", x = "Dyspnea", y = "Age") +
  ggsave(path = "results",
         filename = "03_age_symptoms_death_wuhan.png",
         plot = last_plot(),
         width = 6,
         height = 5)


# Plot 4: Barplot in polar coordinates of incidents per region above 100 -------
df_patient %>%
  group_by(country) %>%
  tally() %>%
  filter(country != "China",n > 100) %>%
  collect() %>%
  ggplot() +
  geom_bar(aes(country, n,fill = country), stat = "identity") +
  coord_polar(start = 300) +
  labs(title = "Numbers of cases (above 100) between Jan-feb 2020",
       subtitle= "COVID-19 affected", x = "", y = "Count") +
  ggsave(path = "results",
         filename = "03_cases_above_hundred.png",
         width = 6,
         height = 5)


# Plot 5: Barplot of the symptoms (when counts > 10 for visual purposes) -------
df_patient %>%
  select(chills:thirst) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE) %>%
  gather(symptoms,counts,chills:thirst) %>%
  filter(counts > 10) %>%
  ggplot(aes(reorder(symptoms,counts),counts,fill = symptoms)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + ylim(0,650) +
  labs(title = "Prevalence of symptoms",
       subtitle= "Observed in more than 10 cases",
       x = "Symptoms", y = "Count") +
  ggsave(path = "results",
         filename = "03_prevalence_symptoms.png",
         plot = last_plot(),
         width = 6,
         height = 5)


# Plot 6: Heatmap of cases -----------------------------------------------------
df_patient %>%
  drop_na(lat,long) %>%
  leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addHeatmap(lng = ~long, lat = ~lat,
             blur = 9, max = 0.05, radius = 6) %>%
  addMarkers(clusterOptions =
               markerClusterOptions())

# labels not possible!
# labs(title = "Confirmed cases",
#     subtitle= "COVID-19 affected",
#     x = "", y = "")

# not able to save by mapview (not working on R 3.6.2)
# tried other packages but did not work


# Visualization: Time Series Data ----------------------------------------------


# FROM HERE - ONLY FROM 2020-03-11 ARE SHOWN (DATE OF DK LOCKDOWN)
# Plot 7: Compare Denmark, Sweden, Romania, Turkey, Philippines per mil.pop ----
df_ts %>%
  filter(region %in% c("Denmark", "Sweden", "Romania",
                       "Turkey", "Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>% # Starting from lockdown
  ggplot() +
  geom_line(aes(date_observation, total_confirmed_per_mil_pop,
                color = region)) +
  labs(title = "Confirmed case(s) per million population",
       subtitle= "COVID-19 affected",
       x = "Date", y = "Count per million population") +
  ggsave(path = "results",
         filename = "03_confirmed_per_mill.png",
         width = 6,
         height = 5)


# Plot 8: Total deaths per mil.pop for the above-mentioned countries -----------
df_ts %>%
  filter(region %in% c("Denmark", "Sweden", "Romania",
                       "Turkey", "Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>% # Starting from lockdown
  ggplot() +
  geom_line(aes(date_observation, total_deaths_per_mil_pop, color = region)) +
  labs(title = "Death(s) per million population",
       subtitle= "COVID-19 affected",
       x = "Date", y = "Count per million population") +
  ggsave(path = "results",
         filename = "03_deaths_per_mill.png",
         width = 6,
         height = 5)



# Model data: Time Series
# ------------------------------------------------------------------------------

# Selecting few countries and nesting per province and time series type
df_ts_selected<- df_ts %>%
  filter(region %in% c("Denmark", "Sweden", "Romania",
                         "Turkey","Philippines"))

# Model data: Time Series ------------------------------------------------------
# Selecting few countries and nesting per region and time series type
df_ts_selected <-
  df_ts %>%
  filter(region %in% c("Denmark", "Sweden", "Romania",
                       "Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>% # Starting from lockdown
  gather(ts, count, total_confirmed:total_deaths_per_mil_pop) %>%
  group_by(region, ts) %>%
  nest()


# Modeling with linear model
df_ts_models <-
  df_ts_selected %>%
  mutate(ts_region = str_c(ts, region, sep="_"),
         mdls = map(data, mdl),
         glance = map(mdls, glance),
         tidy = map(mdls, tidy),
         conf = map(mdls, confint_tidy),
         aug = map(mdls, augment))


# Plot 9: Estimate pr. day of confirmed and death per mil pop -----------------
# Showing model estimate (coefficient) confirmed per mil pop
df_ts_models %>%
  unnest(c(tidy, conf)) %>%
  filter(ts == "total_confirmed_per_mil_pop",term == "date_observation") %>%
  select(region, ts, ts_region, estimate, conf.low, conf.high) %>%
  ggplot(aes(estimate, ts_region, color = ts_region), show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  labs(title = "Model evaluation of confirmed cases",
       subtitle= "COVID-19 affected",
       x = "Estimated coefficient", y = "region") +
  ggsave(path = "results",
         filename = "03_model_eval_confirmed.png",
         width = 10,
         height = 5)


# Plot 10: Showing model estimate (coefficient) confirmed per mil pop ----------
table_df_ts_models_stats <-
  df_ts_models %>%
  unnest(c(tidy, conf)) %>%
  select(region, ts, term:p.value) %>%
  head(10)

linear_models_per_country <-
  grid.arrange(top = "Linear models statistics per country",
               tableGrob(table_df_ts_models_stats))

ggsave(path = "results",
       filename = "03_table_df_ts_models_stats.png",
       plot = linear_models_per_country,
       width = 10,
       height = 5)



# Plot 11: Showing model estimate (coefficient) deaths per mil pop -------------
df_ts_models %>%
  unnest(c(tidy, conf)) %>%
  filter(ts == "total_deaths_per_mil_pop", term == "date_observation") %>%
  select(region, ts, ts_region, estimate,conf.low,conf.high) %>%
  ggplot(aes(estimate, ts_region, color = ts_region), show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin= conf.low, xmax = conf.high)) +
  labs(title = "Model evaluation of death cases",
       subtitle= "COVID-19 affected",
       x = "Estimated coefficient", y = "region") +
  ggsave(path = "results",
         filename = "03_model_eval_death.png",
         width = 10,
         height = 5)


# Plot 12: Evaluation of the models based on the residuals per region ----------
df_ts_models %>%
  unnest(aug) %>%
  select(region, ts, count,.resid) %>%
  filter(ts %in% c("total_confirmed_per_mil_pop",
                   "total_deaths_per_mil_pop")) %>%
  ggplot() +
  geom_boxplot(aes(region,.resid, fill = region)) +
  facet_grid(.~ts) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Model evaluation (residuals)",
       subtitle= "COVID-19",
       x = "region", y = "Residuals") +
  ggsave(path = "results",
         filename = "03_model_eval_residuals.png",
         width = 6,
         height = 5)


# Model data: Patient data -----------------------------------------------------

# Subsetting data frame for the pca (only biological features)
df_patient_pca <-
  df_patient %>%
  select(gender,age, contact_with_Wuhan:is_recovered, chills:thirst) %>%
  na.omit() %>%
  mutate(gender = case_when(gender == 'female' ~ 1,
                            gender == 'male' ~ 0)) %>%
  select_if(~length(unique(.)) > 1) # removing columns with same value


# Plot 13: Making PCA of the subset --------------------------------------------
# Selecting only the binary variables to avoid scale
df_patient_pca %>%
  select(-age) %>%
  prcomp(center = TRUE) %>%
  fviz_eig(main = "PCA of biological features",
           subtitle = "Explained variance in percentage by dimension",
           xlab = "Dimension", ylab = "Percentage") %>%
  ggsave(path = "results",
         filename = "03_pca_biological_features.png",
         width = 6,
         height = 5)


# Creating data frame for decision tree
df_patient_dec <-
  df_patient %>%
  select(gender, age, contact_with_Wuhan:is_recovered,
         chills:thirst) %>%
  select_if(~length(unique(.)) > 1) %>%
  mutate(status = case_when(is_dead == 0 & is_recovered == 0 ~ "still_sick",
                            is_dead == 0 & is_recovered == 1 ~ "recovered",
                            is_dead == 1 & is_recovered == 0 ~ "dead",
                            is_dead == 1 & is_recovered == 1 ~ "dead")) %>%
  mutate(gender = case_when(gender == "female" ~ 1,
                            gender == "male" ~ 0)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate(age = as.integer(age)) %>%
  select(-is_dead, -is_recovered) %>%
  mutate(patient_id = as.character(1:nrow(df_patient))) %>%
  drop_na(status)


set.seed(22100)

# Making train and test for decision tree
df_patient_dec_train <-
  df_patient_dec %>%
  sample_frac(0.8)

df_patient_dec_test <-
  df_patient_dec %>%
  anti_join(df_patient_dec_train, by = "patient_id")


# Fitting the training data
df_patient_dec_fit <-
  df_patient_dec_train %>%
  select(-patient_id) %>%
  rpart(status ~ ., ., method = 'class', model = TRUE,
        minsplit = 1, minbucket = 2, cp = 0.004)


# Plot 14: Plotting the tree ---------------------------------------------------
# Not able to save the image, done manually
rpart.plot(df_patient_dec_fit, roundint = FALSE, extra = "auto")


# Explanation of the tree plot output

# predicted class
# predicted prob for each class
# fraction of observation in the node

# Predicting with the model
df_patient_pred_status <-
  predict(df_patient_dec_fit, df_patient_dec_test,
          type = 'class')


# Defining the true class and predicted class
true_class <-
  df_patient_dec_test %>%
  select(status) %>%
  as_vector()

pred_class <-
  as_vector(df_patient_pred_status)


# Creating confusion matrix
table_cm <-
  as.matrix(confusionMatrix(table(true_class, pred_class)))

table_cm_plot <-
  grid.arrange(top="Confusion Matrix: Decision tree prediction",
               tableGrob(table_cm))

ggsave(path = "results",
       filename = "03_table_cm_plot.png",
       plot = table_cm_plot,
       width = 5,
       height = 4)


# Calculating accuracy
dec_tree_model_acc <- round(sum(diag(table_cm)) / sum(table_cm),3)


# Write data
# ------------------------------------------------------------------------------

#write_tsv(...)
# ggsave(path = "./results",
#        filename = "04_plot.png",
#        plot = bl62_pca_aug_plt,
#        width = 10,
#        height = 6)
