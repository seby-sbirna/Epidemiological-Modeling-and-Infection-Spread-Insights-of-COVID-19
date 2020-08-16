# Clear workspace --------------------------------------------------------------
rm(list = ls())


# Load libraries ---------------------------------------------------------------
library(tidyverse)


# Define functions -------------------------------------------------------------
source(file = "R/99_func.R")


# Load data --------------------------------------------------------------------
daily_covid_trends_df <-
  read_csv('.//data//_clean//daily_covid_trends_df_clean.csv',
           col_types = cols())

patient_data_first_df <-
  read_csv('.//data//_clean//patient_data_first_df_clean.csv',
           col_types = cols())

patient_data_second_df <-
  read_csv('.//data//_clean//patient_data_second_df_clean.csv',
           col_types = cols())

ts_confirmed_world_df <-
  read_csv('.//data//_clean//ts_confirmed_world_df_clean.csv',
           col_types = cols())

ts_confirmed_US_df <-
  read_csv('.//data//_clean//ts_confirmed_US_df_clean.csv',
           col_types = cols())

ts_deaths_world_df <-
  read_csv('.//data//_clean//ts_deaths_world_df_clean.csv',
           col_types = cols())

ts_deaths_US_df <-
  read_csv('.//data//_clean//ts_deaths_US_df_clean.csv',
           col_types = cols())

ts_recovered_world_df <-
  read_csv('.//data//_clean//ts_recovered_world_df_clean.csv',
           col_types = cols())

population_by_country_df <-
  read_csv('.//data//_clean//population_by_country_df_clean.csv',
           col_types = cols())



# AUGMENTING patient_data_first_df & patient_data_second_df --------------------
# CREATE AGE GROUPS for the two dataframes ------------------------------------
patient_data_first_df <-
  patient_data_first_df %>%
  mutate(age_group = case_when(between(age, 0, 4) ~ '00-04',
                               between(age, 5, 9) ~ '05-09',
                               between(age, 10, 14) ~ '10-14',
                               between(age, 15, 19) ~ '15-19',
                               between(age, 20, 29) ~ '20-29',
                               between(age, 30, 39) ~ '30-39',
                               between(age, 40, 49) ~ '40-49',
                               between(age, 50, 59) ~ '50-59',
                               between(age, 60, 69) ~ '60-69',
                               age >= 70 ~ '70+')) %>%
  select(date_observation:age, age_group, everything())

patient_data_second_df <-
  patient_data_second_df %>%
  mutate(age_group = case_when((between(age_dbl, 0, 4) |
                                  age == '0-4') ~ '00-04',
                               (between(age_dbl, 5, 9) |
                                  age == '5-9') ~ '05-09',
                               (between(age_dbl, 10, 14) |
                                  age == '10-14') ~ '10-14',
                               (between(age_dbl, 15, 19) |
                                  age == '15-19') ~ '15-19',
                               (between(age_dbl, 20, 29) |
                                  age == '20-29') ~ '20-29',
                               (between(age_dbl, 30, 39) |
                                  age == '30-39') ~ '30-39',
                               (between(age_dbl, 40, 49) |
                                  age == '40-49') ~ '40-49',
                               (between(age_dbl, 50, 59) |
                                  age == '50-59') ~ '50-59',
                               (between(age_dbl, 60, 69) |
                                  age == '60-69') ~ '60-69',
                               ((age_dbl >= 70)|
                                  (age == '70-79')|
                                  (age == '80-89')) ~ '70+')) %>%
  mutate(age = age_dbl) %>%
  select(date_observation:gender, age, age_group, date_onset:long)


# COMBINE THE TWO PATIENT DATASETS INTO ONE ------------------------------------
final_patient_data_df <-
  patient_data_first_df %>%
  full_join(patient_data_second_df,
            by = c("date_observation", "province", "country", "gender",
                   "age", "age_group", "date_onset", "date_admission_hospital",
                   "contact_with_Wuhan", "is_dead", "is_recovered",
                   "symptoms_set")) %>%
  arrange(date_observation)


# CREATE CATEGORICAL columns for each unique symptom, turn into wide format ----
final_patient_data_df <-
  final_patient_data_df %>%

  # Add a surrogate key representing the row number, so that,
  # after converting first to long format, we can still widen the data back into
  # rows that corresponded to the same observation

  mutate(surrogate_key = row_number()) %>%

  # For every symptom that contains a space inside its name (eg. 'sore throat'),
  # replace the whitespace with a '_' sign for column naming

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '([^,])( )', replacement = '\\1_')) %>%

  # Separates the multiple symptoms into 6 different columns
  # (because 6 is the maximum number of concurrent symptoms
  # that we have in this dataset)
  # Will give a desirable warning:  Missing pieces filled with `NA`

  separate(symptoms_set,
           into = c('s1', 's2', 's3', 's4', 's5', 's6'),
           sep = ', ') %>%

  # We need to create the dummy variables for each unique individual symptom,
  # but to do that, we must first have a column with one symptom per entry.
  # Therefore, we must first tidy the data into long format

  pivot_longer(cols = c('s1', 's2', 's3', 's4', 's5', 's6'),
               names_to = 'sX',
               values_to = 'symptoms') %>%

  # After this, we will actually never use the 'names_to' column ('sX'),
  # since that was just a placeholder for the long format of our data

  select(-sX) %>%

  # Before we create our dummy variables by tidying the data into wide format,
  # we must first create a "values_from" column,
  # so that the new symptom columns will take their data from somewhere

  mutate(dummy = 1) %>%

  # Now, we create columns for each individual symptom found in the 'names_from'
  # column (i.e. 'symptoms' column), and it will be filled with 1
  # ('values_from' column) if the value exists, otherwise it will return NULL.
  # In case it returns NULL, the 'values_fill' parameter modifies the 'dummy'
  # column to become 0 in that case, thus filling the columns with 0 if the
  # symptom is not present The 'values_fn' function allows us to specify what
  # happens when, for the same observation, more than one value resides
  # in the 'values_from' column

  pivot_wider(names_from = symptoms,
              values_from = dummy,
              values_fill = list(dummy = 0),
              names_sep = '_',
              values_fn = list(dummy = min)) %>%

  # Finally, remove the 'surrogate_key' and 'NA' column

  select(-c('surrogate_key', 'NA'))


# CONVERT COLS TO FACTOR -------------------------------------------------------
# The columns 'gender' and 'age_group' are categorical columns
final_patient_data_df <-
  final_patient_data_df %>%
  mutate(gender = factor(gender, levels = c('male', 'female'))) %>%
  mutate(age_group = factor(age_group,
                            levels = c("00-04", "05-09",
                                       "10-14", "15-19",
                                       "20-29", "30-39",
                                       "40-49", "50-59",
                                       "60-69", "70+")))



# AUGMENTING NON-US TIMESERIES DF (ts_..._df) ----------------------------------
# COMBINE THE THREE TIME-SERIES DATASETS INTO ONE ------------------------------

# Rename the 'cases' column in each time-series DF, so as to facilitate joining
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  rename('total_confirmed' = 'cases')

ts_deaths_world_df <-
  ts_deaths_world_df %>%
  rename('total_deaths' = 'cases')

ts_recovered_world_df <-
  ts_recovered_world_df %>%
  rename('total_recovered' = 'cases')

# Since the data has exactly the same number of columns and observations
# then we can safely join all three dataframes into one

final_ts_world_df <-
  ts_confirmed_world_df %>%
  left_join(ts_deaths_world_df,
            by = c("province", "country",
                   "lat", "long", "date_observation")) %>%
  left_join(ts_recovered_world_df,
            by = c("province", "country",
                   "lat", "long", "date_observation")) %>%
  mutate(total_recovered = replace_na(total_recovered, 0))


# AUGMENT THE TIME-SERIES WITH POPULATION DATA ---------------------------------
final_ts_world_df <-
  final_ts_world_df %>%

  # Join the final time-series data together with the population data by country
  left_join(population_by_country_df,
            by = 'country') %>%

  # Create new variables calculating the number of new cases per day
  mutate(new_confirmed = total_confirmed - lag(total_confirmed,
                                               default = 0)) %>%
  mutate(new_deaths    = total_deaths    - lag(total_deaths,
                                               default = 0)) %>%
  mutate(new_recovered = total_recovered - lag(total_recovered,
                                               default = 0)) %>%

  # Reset the default lag value to 0 when switching to a new province or country
  mutate(new_confirmed = if_else(new_confirmed < 0,
                                 true = 0,
                                 false = new_confirmed)) %>%
  mutate(new_deaths    = if_else(new_deaths < 0,
                                 true = 0,
                                 false = new_deaths)) %>%
  mutate(new_recovered = if_else(new_recovered < 0,
                                 true = 0,
                                 false = new_recovered)) %>%

  # Create new variables representing
  # the number of people infected per/in 1 million persons
  # (considering the country population!)
  mutate(total_confirmed_per_mil_pop =
           total_confirmed / (total_population/1000000)) %>%
  mutate(total_deaths_per_mil_pop =
           total_deaths / (total_population/1000000)) %>%

  # Create variables representing the number of active and closed cases per day
  mutate(closed_cases = as.double(total_deaths + total_recovered)) %>%
  mutate(active_cases = as.double(total_confirmed - closed_cases)) %>%

  # Reorder the variables to a useful form
  select(province:total_recovered, new_confirmed:new_recovered,
         active_cases, closed_cases, total_population,
         total_confirmed_per_mil_pop, total_deaths_per_mil_pop) %>%

  # Summarising
  select(province, country, date_observation,
         total_confirmed:total_deaths_per_mil_pop) %>%
  group_by(date_observation, province) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  rename('region' = 'province')



# CREATE SIR modelling dataframe -----------------------------------------------
final_ts_world_df <-
  final_ts_world_df %>%
  select(region, date_observation,
         total_confirmed:total_deaths_per_mil_pop) %>%
  group_by(region, date_observation) %>%
  summarise_if(is.numeric,funs(sum)) %>%

  group_by(region) %>%
  mutate(tmp_date = case_when(total_confirmed > 0 ~ date_observation)) %>%
  mutate(days_since_first = date_observation - min(tmp_date, na.rm = TRUE)) %>%
  ungroup %>%
  select(-tmp_date) %>%
  mutate(days_since_first = as.numeric(days_since_first,units="days"))


SIR_df <-
  final_ts_world_df %>%
  rename(N = total_population) %>%
  mutate(I = total_confirmed - total_recovered - total_deaths) %>%
  mutate(R = total_recovered + total_deaths) %>%
  mutate(S = N - I - R) %>%
  select(region, date_observation, days_since_first, S, I, R, N)



# Write data -------------------------------------------------------------------
write_csv(x = final_patient_data_df,
          path = ".//data//_augmented//final_patient_data_df_augm.csv")
write_csv(x = final_ts_world_df,
          path = ".//data//_augmented//final_ts_world_df_augm.csv")
write_csv(x = SIR_df,
          path = ".//data//_augmented//SIR_df.csv")
