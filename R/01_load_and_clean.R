# Clear workspace --------------------------------------------------------------
rm(list = ls())


# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)


# Define functions -------------------------------------------------------------
source(file = "R/99_func.R")


# Load data --------------------------------------------------------------------
daily_covid_trends_df <-
  read_csv('.//data//_raw//covid_19_data.csv',
           col_types = cols())

patient_data_first_df <-
  read_csv('.//data//_raw//COVID19_line_list_data.csv',
           col_types = cols())

patient_data_second_df <-
  read_csv('.//data//_raw//COVID19_open_line_list.csv',
           col_types = cols())

ts_confirmed_world_df <-
  read_csv('.//data//_raw//time_series_covid_19_confirmed.csv',
           col_types = cols())

ts_confirmed_US_df <-
  read_csv('.//data//_raw//time_series_covid_19_confirmed_US.csv',
           col_types = cols())

ts_deaths_world_df <-
  read_csv('.//data//_raw//time_series_covid_19_deaths.csv',
           col_types = cols())

ts_deaths_US_df <-
  read_csv('.//data//_raw//time_series_covid_19_deaths_US.csv',
           col_types = cols())

ts_recovered_world_df <-
  read_csv('.//data//_raw//time_series_covid_19_recovered.csv',
           col_types = cols())

population_by_country_df <-
  read_csv('.//data//_raw//wpp2019_total_population.csv',
           col_types = cols())



# Wrangle data from 'daily_covid_trends_df': covid_19_data.csv -----------------
print('The structure of daily_covid_trends_df is: ')
str(daily_covid_trends_df)


# RENAME COLUMNS ---------------------------------------------------------------
daily_covid_trends_df <-
  daily_covid_trends_df %>%
  select(-SNo) %>%
  mutate(ObservationDate = mdy(ObservationDate)) %>%
  rename("date_observation" = "ObservationDate") %>%
  rename("province" = `Province/State`) %>%
  rename("country" = `Country/Region`) %>%
  select(-`Last Update`) %>%
  rename("total_confirmed" = 'Confirmed') %>%
  rename("total_deaths" = 'Deaths') %>%
  rename("total_recovered" = 'Recovered')


# Standarize PROVINCE data (removing data inconstencies) -----------------------
daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert patterns of the type 'Pierce County, WA' to 'Pierce County'
  mutate(province = str_replace_all(
    province, pattern = '(.*), \\w\\.?\\w\\.?(.*)', replacement = '\\1\\2')) %>%

  # Convert patterns of the type 'Pierce County' to 'Pierce'
  mutate(province = str_replace_all(
    province, pattern = '(.*) County(.*)', replacement = '\\1\\2')) %>%

  # Fix problems associated with the Shanxi province
  mutate(province = str_replace_all(
    province, pattern = 'Shaanxi', replacement = 'Shanxi')) %>%

  # Fix problems associated with UK
  mutate(province = str_replace_all(
    province, pattern = 'United Kingdom', replacement = 'UK')) %>%

  # Convert patterns like 'Travis (From Diamond Princess)' to 'Diamond Princess'
  mutate(province = str_replace_all(
    province, pattern = '.*\\(From Diamond Princess\\)',
    replacement = 'Diamond Princess')) %>%

  # Convert ('Diamond Princess cruise ship' or
  #'From Diamond  Princess' or 'Cruise Ship') to 'Diamond Princess'
  mutate(province = str_replace_all(
    province, pattern = '(Diamond Princess cruise ship)|
    (From Diamond Princess)|(Cruise Ship)',
    replacement = 'Diamond Princess')) %>%

  # Convert 'Grand Princess Cruise Ship' to 'Grand Princess'
  mutate(province = str_replace_all(
    province, pattern = 'Grand Princess Cruise Ship',
    replacement = 'Grand Princess')) %>%

  # Convert 'Grand Princess Diamond Princess' to 'Unassigned Location'
  mutate(province = str_replace_all(
    province, pattern = 'Grand Princess Diamond Princess',
    replacement = 'Unassigned Location')) %>%

  # Convert 'Unknown Location' to 'Unassigned Location'
  mutate(province = str_replace_all(
    province, pattern = 'Unknown Location',
    replacement = 'Unassigned Location')) %>%

  # Replace NA values with 'Unassigned Location'
  mutate(province = replace_na(province, 'Unassigned Location')) %>%

  # Remove data about Diamond Princess, Grand Princess and MS Zandaam,
  # since they are not belonging to any actual countries
  filter(province %in% c('Diamond Princess', 'Grand Princess') == FALSE)


# Standarize COUNTRY data (removing data inconstencies -------------------------
daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert 'Mainland China' to 'China', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Mainland China', replacement = 'China')) %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(
    country, pattern = 'Burma', replacement = 'Myanmar')) %>%

  # Convert "('St. Martin',)" to 'St. Martin'
  mutate(country = str_replace_all(
    country, pattern = "\\(\\'St. Martin\\',\\)", replacement = 'St. Martin')
  ) %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(country = str_replace_all(
    country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))',
    replacement = 'Congo')) %>%

  # Convert 'Bahamas, The' AND 'The Bahamas' to 'Bahamas'
  mutate(country = str_replace_all(
    country, pattern = '.*Bahamas.*', replacement = 'Bahamas')) %>%

  # Convert 'Gambia, The' AND 'The Gambia' to 'Gambia'
  mutate(country = str_replace_all(
    country, pattern = '.*Gambia.*', replacement = 'Gambia')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(country = str_replace_all(
    country, pattern = 'Holy See', replacement = 'Vatican City')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Arab Emirates', replacement = 'UAE')) %>%

  # Convert 'Diamond Princess' and 'MS Zaandam' Country to 'Others'
  mutate(country = str_replace_all(
    country, pattern = '(Diamond Princess)|(MS Zaandam)',
    replacement = 'Others')) %>%

  # Remove data about 'Others' country, ie from Diamond Princess, Grand Princess
  # and MS Zandaam, since they are not belonging to any actual countries
  filter(country != 'Others')


# Transform MISSING VALUES in PROVINCE column ----------------------------------
# Here, if the province is unknown, we just assign it to the name of the country
daily_covid_trends_df <-
  daily_covid_trends_df %>%
  mutate(province = if_else(province == 'Unassigned Location',
                            true = country,
                            false = province))



# Wrangle data from 'patient_data_first_df': COVID19_line_list_data.csv --------
print('The structure of patient_data_first_df is: ')
str(patient_data_first_df)


# REMOVING UNNECESSARY COLUMNS -------------------------------------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Remove empty columns
  select(-c(X4, X22, X23, X24, X25, X26, X27)) %>%

  # Remove 'summary' column, since data is summarized through the other columns
  select(-summary) %>%

  # Remove non-relevant columns for our analysis
  select(-c(id, source, link, If_onset_approximated, case_in_country))


# RENAME AND WRANGLE COLUMNS ---------------------------------------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Rename the province
  rename("province" = "location") %>%

  # Rename the date of reported infection and convert to date
  mutate(`reporting date` = mdy(`reporting date`)) %>%
  rename("date_observation" = `reporting date`) %>%

  # Rename the date of onset and convert to date
  rename("date_onset" = "symptom_onset") %>%
  mutate(date_onset = mdy(date_onset)) %>%

  # Rename the the date of hospital admission and convert to 'date'
  rename('date_admission_hospital' = 'hosp_visit_date') %>%
  mutate(date_admission_hospital = mdy(date_admission_hospital)) %>%

  # Rename the dates for exposure start and end, and convert them to 'date'
  mutate(exposure_start = mdy(exposure_start)) %>%
  mutate(exposure_end = mdy(exposure_end)) %>%
  rename('date_exposure_start' = 'exposure_start') %>%
  rename('date_exposure_end' = 'exposure_end') %>%

  # Rename and combine the variables related to Wuhan contact
  rename('visited_Wuhan' = `visiting Wuhan`) %>%
  rename('lives_in_Wuhan' = `from Wuhan`) %>%
  mutate(lives_in_Wuhan = replace_na(lives_in_Wuhan, 0)) %>%
  mutate(contact_with_Wuhan = if_else( visited_Wuhan + lives_in_Wuhan > 0,
                                       true = 1, false = 0)) %>%

  # Remove the old Wuhan-related columns and reorder columns in the dataframe
  select(-c(visited_Wuhan, lives_in_Wuhan)) %>%
  select(1:9, 13, 10:12) %>%

  # Rename and fix data errors for the death and recovered logical variables
  rename("is_dead" = "death") %>%
  rename("is_recovered" = "recovered") %>%
  mutate(is_recovered = if_else(is_recovered != 0, true = 1, false = 0)) %>%
  mutate(is_dead = if_else(is_dead != 0, true = 1, false = 0)) %>%

  # Renaming the symptoms set
  rename("symptoms_set" = "symptom")


# Standarize PROVINCE data (removing data inconstencies) -----------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Convert patterns of the type: 'Hechi, Guangxi' to 'Guangxi'
  mutate(province = str_replace_all(province, pattern = '(.*), (.*)',
                                    replacement = '\\2')) %>%

  # Convert patterns like: 'Fukuoka Prefecture' or 'Fukuoka City' to 'Fukuoka'
  mutate(province = str_replace_all(province,
                                    pattern = '(.*) ((Prefecture)|(City))',
                                    replacement = '\\1')) %>%

  # Fix problems associated with the Shanxi province
  mutate(province = str_replace_all(province, pattern = '(.*) \\(.*\\)',
                                    replacement = '\\1')) %>%
  mutate(province = str_replace_all(province, pattern = 'Shaanxi',
                                    replacement = 'Shanxi'))


# Standarize COUNTRY data (removing data inconstencies -------------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Convert 'USA' to 'US', for consistency with the other datasets
  mutate(country = str_replace_all(country, pattern = 'USA',
                                   replacement = 'US')) %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(country, pattern = 'Burma',
                                   replacement = 'Myanmar')) %>%

  # Properly adjust country for Macau, so that it is 'Macau'
  # and not 'China' (consistent with the other datasets)
  mutate(country = if_else((province == 'Macau'),
                           true = 'Macau',
                           false = country))


# Fix small values of AGE data -------------------------------------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Fix any age values smaller than one to be 1 years old
  mutate(age = if_else(age < 1, true = 1, false = age))


# Wrangle the SYMPTOMS_SET string data -----------------------------------------
patient_data_first_df <-
  patient_data_first_df %>%

  # Fix patterns of the type 'X with Y' to 'X, Y'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(.*) with (.*)', replacement = '\\1, \\2')) %>%

  # Fix problems associated with 'chest pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(chest) discomfort', replacement = '\\1 pain')) %>%

  # Fix problems associated with 'dyspnea' (i.e. shortness of breath)
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(shortness of breath)|(difficulty breathing)|
    (difficult in breathing)|(breathlessness)|(respiratory distress)',
    replacement = 'dyspnea')) %>%

  # Fix problems associated with 'chills'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(chill,)|(cold,)', replacement = 'chills,')) %>%

  # Fix problems associated with 'sore throat'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(itchy throat)|(throat discomfort)|(throat pain)',
    replacement = 'sore throat')) %>%

  # Fix problems associated with 'fever'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(feaver)|(feve\\\\)|(high fever)|(mild fever)',
    replacement = 'fever')) %>%

  # Fix problems associated with 'muscle pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(muscle ((cramps)|(aches)))|(myalgia(s)?)|
    (aching muscles)|(sore body)|(physical discomfort)',
    replacement = 'muscle pain')) %>%

  # Fix problems associated with 'heavy head'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'heavy head',
    replacement = 'headache, nausea')) %>%

  # Fix problems associated with 'runny nose' (i.e. "nasal discharge" or "snot"
  # means mucus from the nose) [vs (sputum = mucus from the airways)]
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(runny nose)|(sneeze)',
    replacement = 'nasal discharge')) %>%

  # Fix problems associated with 'flu'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'flu symptoms',
    replacement = 'flu')) %>%

  # Fix problems associated with 'cough'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(mild )?cough(ing)?', replacement = 'cough')) %>%

  # Fix problems associated with 'fatigue'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(tired)', replacement = 'fatigue'))



# Wrangle data from 'patient_data_second_df': COVID19_open_line_list.csv -------
print('The structure of patient_data_second_df is: ')
str(patient_data_second_df)


# REMOVING UNNECESSARY COLUMNS -------------------------------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Remove highly sparse data columns
  select(-c(starts_with('travel_'),
            reported_market_exposure, starts_with('chronic_'),
            sequence_available, notes_for_discussion,
            `wuhan(0)_not_wuhan(1)`)) %>%

  # Remove empty columns
  select(-c(X34:X45)) %>%

  # Remove non-relevant columns for our analysis
  select(-c(ID, city, geo_resolution, additional_information, source)) %>%
  select(age : date_death_or_discharge)


# Fix MISSING VALUES in all columns --------------------------------------------
patient_data_second_df <-
  patient_data_second_df %>%
  na_if('N/A') %>%
  na_if('#N/A')


# RENAME AND WRANGLE COLUMNS ---------------------------------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Rename the gender column
  rename("gender" = "sex") %>%

  # Fix the Male/male & Female/female case errors
  mutate(gender = str_to_lower(gender)) %>%

  # Rename latitude and longitude and convert to double
  rename("lat" = "latitude") %>%
  rename("long" = "longitude") %>%
  mutate(lat = as.double(lat)) %>%
  mutate(long = as.double(long)) %>%

  # Rename the date of reported infection and convert to date
  rename("date_observation" = "date_confirmation") %>%
  mutate(date_observation = dmy(date_observation)) %>%

  # Rename the date of symptom onset and convert to date
  rename("date_onset" = "date_onset_symptoms") %>%
  mutate(date_onset = dmy(date_onset)) %>%

  # Convert the date of hospital admissionto 'date'
  mutate(date_admission_hospital = dmy(date_admission_hospital)) %>%

  # Rename and combine the variables related to Wuhan contact
  mutate(lives_in_Wuhan = replace_na(lives_in_Wuhan, 0)) %>%
  mutate(contact_with_Wuhan = case_when(
    str_detect(lives_in_Wuhan,
               pattern = ('(0)|([nN]o)|(Chinese)|(live(d)? in.*)|
                          (thai.*)|(used to be)|(.*resident.*)')
    ) ~ 0, TRUE ~ 1)) %>%

  # Remove the old Wuhan-related columns
  select(-c(lives_in_Wuhan)) %>%

  # Create new variables for checking whether a person is recovered or dead
  mutate(is_recovered = if_else(condition = (
    str_detect(outcome, pattern = '([Dd]ischarge.*)|(recovered)') &
      is.na(date_death_or_discharge) == FALSE),
    true = 1, false = 0)) %>%

  mutate(is_dead = if_else(condition = (
    str_detect(outcome, pattern = '(death)|(died)') &
      is.na(date_death_or_discharge) == FALSE),
    true = 1, false = 0)) %>%

  select(-c(outcome, date_death_or_discharge)) %>%

  # Renaming the symptoms set
  rename("symptoms_set" = "symptoms") %>%

  # Re-order the columns as presented in patient_data_first_df
  # (dplyr's one_of() is really good because it will ignore the columns
  # that are unknown to the new DF)
  # This will generate a warning that we are aware of and is desirable:
  # Unknown columns: `date_exposure_start`, `date_exposure_end`
  select(one_of(colnames(patient_data_first_df)), everything())


# Standarize PROVINCE data (removing data inconstencies ------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Convert patterns like: 'Fukuoka Prefecture' or 'Fukuoka City' to 'Fukuoka'
  mutate(province = str_replace_all(
    province, pattern = '(.*) ((Prefecture)|(City))', replacement = '\\1')) %>%

  # Fix problems associated with the Shanxi province
  mutate(province = str_replace_all(
    province, pattern = '(.*) \\(.*\\)', replacement = '\\1')) %>%

  mutate(province = str_replace_all(
    province, pattern = 'Shaanxi', replacement = 'Shanxi'))


# Standarize COUNTRY data (removing data inconstencies -------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(
    country, pattern = 'Burma', replacement = 'Myanmar')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Arab Emirates', replacement = 'UAE')) %>%

  # Convert 'United Kingdom' to 'UK', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Kingdom', replacement = 'UK')) %>%

  # Convert 'United States' to 'US', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United States', replacement = 'US')) %>%

  # Properly adjust country for Macau, so that it is 'Macau' and
  # not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Macau') ~ 'Macau',
                             TRUE ~ country)) %>%

  # Properly adjust country for Hong Kong,
  # so that it is 'Hong Kong' and not 'China' (consistent with other datasets)
  mutate(country = case_when((province == 'Hong Kong') ~ 'Hong Kong',
                             TRUE ~ country))


# Replace missing values in PROVINCE & COUNTRY data ----------------------------
patient_data_second_df <-
  patient_data_second_df %>%
  mutate(province = if_else(is.na(province) == TRUE,
                            true = country, false = province)) %>%
  mutate(country = if_else(is.na(country) == TRUE,
                           true = province, false = country)) %>%

  # Remove 953 rows with completely missing data
  # province can now only be NA if country is also NA
  filter(is.na(province) == FALSE)


# Fix small values of AGE data -------------------------------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Make a new variable containing only the values of age, converted to double
  # This will generate the desirable warning: NAs introduced by coercion
  mutate(age_dbl = as.double(age)) %>%

  # Reorder the columns in a relevant way
  select(date_observation:gender, age_dbl, everything()) %>%

  # Fix any age values with decimals to be rounded to full numbers
  mutate(age_dbl = round(age_dbl))


# Wrangle the SYMPTOMS_SET string data -----------------------------------------
patient_data_second_df <-
  patient_data_second_df %>%

  # Fix comma & formatting issues
  mutate(symptoms_set = str_replace_all(symptoms_set, ';', ',')) %>%
  mutate(symptoms_set = str_replace_all(symptoms_set, '(, )\\W+', '\\1')) %>%
  mutate(symptoms_set = str_replace_all(symptoms_set, ' and ', ', ')) %>%
  mutate(symptoms_set = str_replace_all(symptoms_set,
                                        '(\\w),(\\w)', '\\1, \\2')) %>%

  # Convert all symptoms to lower-case
  mutate(symptoms_set = str_to_lower(symptoms_set)) %>%

  # Fix specific patterns related to not having or unidentifiable symptoms
  mutate(symptoms_set = na_if(
    symptoms_set, 'asymptomatic')) %>%
  mutate(symptoms_set = na_if(
    symptoms_set, 'no serious symptoms')) %>%
  mutate(symptoms_set = na_if(
    symptoms_set, 'no symptoms')) %>%
  mutate(symptoms_set = na_if(
    symptoms_set, 'yes')) %>%
  mutate(symptoms_set = na_if(
    symptoms_set, 'similar to a respiratory infection')) %>%
  mutate(symptoms_set = na_if(
    symptoms_set, 'lesions on chest radiographs')) %>%

  # Fix other specific errors related to symptoms
  mutate(symptoms_set = str_replace_all(
    symptoms_set, ', other symptoms', '')) %>%
  mutate(symptoms_set = str_replace_all(
    symptoms_set, ', eventually showed.*', '')) %>%
  mutate(symptoms_set = str_replace_all(
    symptoms_set, 'no respiratory symptoms, ', '')) %>%
  mutate(symptoms_set = str_replace_all(
    symptoms_set, '(, )lesions', '')) %>%
  mutate(symptoms_set = str_replace_all(
    symptoms_set, 'fever 38.1[^,].*cough', 'fever, cough')) %>%

  # Fix problems associated with 'chest pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(chest ((distress)|(tightness)))
    |(pleuritic chest pain)',
    replacement = 'chest pain')) %>%

  # Fix problems associated with 'dyspnea' (i.e. shortness of breath)
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(anhelation)',
    replacement = 'dyspnea')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'severe (dyspnea)',
    replacement = '\\1')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((breathing difficulty)|(difficulty breathing))',
    replacement = 'dyspnea')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'shortness( of)? breath',
    replacement = 'dyspnea')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(anhelation)',
    replacement = 'dyspnea')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'respiratory ((distress)|(problems)|(symptoms))',
    replacement = 'dyspnea')) %>%

  # Fix problems associated with 'chills'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(cold)|(rigor)',
    replacement = 'chills')) %>%

  # Fix problems associated with 'sore throat'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(acute pharyngitis)|(pharynx)|(pharyngalgia)|
    (pharyngeal ((discomfort)|(dryness)))',
    replacement = 'sore throat')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(sore throa$)|(throat discomfort)|(dry throat)',
    replacement = 'sore throat')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'sore throat, sore throat',
    replacement = 'sore throat')) %>%

  # Fix problems associated with 'fever'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(((low)|(high)) fever)',
    replacement = 'fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(( )?)(fever.*\\(.*\\))',
    replacement = '\\1fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(fever.*)(,)', replacement = 'fever\\2')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '^fever[^,]*$', replacement = 'fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'rever', replacement = 'fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '^mild$', replacement = 'fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'fever[^,]*$', replacement = 'fever')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '37.1 ° c', replacement = 'fever')) %>%

  # Fix problems associated with 'diarrhea'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'diarrh((oea)|(eoa))',
    replacement = 'diarrhea')) %>%

  # Fix problems associated with 'headache'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(headache)\\.', replacement = '\\1')) %>%

  # Fix problems associated with 'joint pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'difficulty walking',
    replacement = 'joint pain')) %>%

  # Fix problems associated with 'malaise'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(physical discomfort)|(poor physical condition)',
    replacement = 'malaise')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(discomfort)|(general malaise)',
    replacement = 'malaise')) %>%

  # Fix problems associated with 'muscle pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'musc((le )|(ular ))((soreness)|
    (ache(s)?)|(stiffness))',
    replacement = 'muscle pain')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(myalgia(s)?)|(aching muscles)|
    (sore ((body)|(muscle)))|(soreness)',
    replacement = 'muscle pain')) %>%

  # Fix problems associated with 'nasal discharge'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((runny nose)|(sneeze)|(sneezing)|
    (^discharge)|(nasal congestion))',
    replacement = 'nasal discharge')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'rhinorrh((oea)|(ea))',
    replacement = 'nasal discharge')) %>%

  # Fix problems associated with 'flu'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((flu-like symptoms)|(feeling ill))',
    replacement = 'flu')) %>%

  # Fix problems associated with 'cough'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(mild )?cough(ing)?',
    replacement = 'cough')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(dry )?cough( symptoms)?',
    replacement = 'cough')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'couh',
    replacement = 'cough')) %>%

  # Fix problems associated with 'fatigue'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((systemic )?weakness)',
    replacement = 'fatigue')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(weak)', replacement = 'fatigue')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(transient fatigue)', replacement = 'fatigue')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(lack of energy)', replacement = 'fatigue')) %>%

  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '(full body slump)', replacement = 'fatigue')) %>%

  # Fix problems associated with 'pneumonia'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((pulmonary inflammation)|
    (severe pneumonia)|(pneumonitis))',
    replacement = 'pneumonia')) %>%

  # Fix problems associated with 'reflux'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'esophageal (reflux)',
    replacement = '\\1')) %>%

  # Fix problems associated with 'sputum'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '((expectoration)|(phlegm))',
    replacement = 'sputum')) %>%

  # Fix problems associated with 'loss of appetite'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'anorexia',
    replacement = 'loss of appetite')) %>%

  # Fix problems associated with 'nausea'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'dizziness',
    replacement = 'nausea')) %>%

  # Fix problems associated with 'leg pain'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'sore limbs',
    replacement = 'leg pain')) %>%

  # Fix problems associated with 'conjunctivitis'
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'eye irritation',
    replacement = 'conjunctivitis')) %>%

  # Fix problems associated with ARIs (acute respiratory infection)
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = 'acute respiratory viral infection',
    replacement = 'acute respiratory infection')) %>%

  # Fix a problem related to a stray fever value being present
  mutate(symptoms_set = str_replace_all(
    symptoms_set, pattern = '37\\.1.*,',
    replacement = ''))



# Wrangle df 'ts_confirmed_world_df': time_series_covid_19_confirmed.csv -------
print('The structure of ts_confirmed_world_df is: ')
str(ts_confirmed_world_df)


# RENAME COLUMNS ---------------------------------------------------------------
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%

  # Rename the province
  rename("province" = `Province/State`) %>%

  # Rename the country
  rename("country" = `Country/Region`) %>%

  # Rename the Lat and Long variables to lower-case, as good practice
  rename('lat' = 'Lat') %>%
  rename('long' = 'Long')


# Fix values in PROVINCE column ------------------------------------------------
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  mutate(province = str_replace_all(
    province, pattern = 'Shaanxi',
    replacement = 'Shanxi')) %>%
  filter(province %in%
           c('Diamond Princess', 'Grand Princess', 'MS Zaandam') == FALSE) %>%
  na_if('Bonaire, Sint Eustatius and Saba')


# Fix values in COUNTRY column -------------------------------------------------
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(country = str_replace_all(
    country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))',
    replacement = 'Congo')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(country = str_replace_all(
    country, pattern = 'Holy See',
    replacement = 'Vatican City')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Arab Emirates',
    replacement = 'UAE')) %>%

  # Convert 'United Kingdom' to 'UK', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Kingdom',
    replacement = 'UK')) %>%

  # Convert 'Cote d'Ivoire' to 'Ivory Coast' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Cote d\'Ivoire',
    replacement = 'Ivory Coast')) %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(
    country, pattern = 'Burma',
    replacement = 'Myanmar')) %>%

  # Convert 'Korea, South' to 'South Korea' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Korea), (South)',
    replacement = '\\2 \\1')) %>%

  # Convert 'Taiwan*' to 'Taiwan', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Taiwan)\\*',
    replacement = '\\1')) %>%

  # Convert 'Czechia' to 'Czech Republic' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Czechia',
    replacement = 'Czech Republic')) %>%

  # Remove entries related to 'Diamond Princess', 'Grand Princess', 'MS Zaandam'
  filter(country %in%
           c('Diamond Princess', 'Grand Princess', 'MS Zaandam') == FALSE) %>%

  # Properly adjust country for Macau, so that it is 'Macau'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Macau') ~ 'Macau',
                             TRUE ~ country)) %>%

  # # Properly adjust country for Hong Kong, so that it is 'Hong Kong'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Hong Kong') ~ 'Hong Kong',
                             TRUE ~ country))


# Transform MISSING VALUES in PROVINCE column ----------------------------------
# If the province is unknown, we just assign it to the name of the country
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  mutate(province = if_else(is.na(province) == TRUE,
                            true = country,
                            false = province))


# TIDY the dataset into LONG FORMAT --------------------------------------------
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  pivot_longer(-c(1:4),
               names_to = 'date_observation',
               values_to = 'cases')


# CONVERT DATES from chr to date -----------------------------------------------
ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  mutate(date_observation = mdy(date_observation))


# AGGREGATE CASES for Canada ---------------------------------------------------
# Canada is the only country in the dataset that has a problem,
# its values for 'confirmed' and 'deaths' are aggregated within provinces,
# however the values for 'recovered' are aggregated across the country
# Therefore, we will standardize the case aggregation to be across the country

canada_confirmed_subset <-
  ts_confirmed_world_df %>%

  # Remove all provinces from Canada
  filter(country == 'Canada') %>%
  mutate(province = 'Canada') %>%

  # Summarize the total number of cases over the entire country
  group_by(date_observation) %>%
  mutate(cases = sum(cases)) %>%

  # Only keep 1 aggregated observation for every observation day
  distinct(date_observation, .keep_all = TRUE)


# Join the new wrangled data for Canada with the old dataframe,
# from which we have removed the observations related to Canada,
# after which we arrange ascendingly by country, then by province

ts_confirmed_world_df <-
  ts_confirmed_world_df %>%
  filter(country != 'Canada') %>%
  full_join(canada_confirmed_subset,
            by = c("province", "country", "lat", "long",
                   "date_observation", "cases")) %>%
  arrange(country, province)



# Wrangle DF 'ts_confirmed_US_df': time_series_covid_19_confirmed_US.csv -------
print('The structure of ts_confirmed_US_df is: ')
str(ts_confirmed_US_df)


# REMOVING UNNECESSARY COLUMNS -------------------------------------------------
ts_confirmed_US_df <-
  ts_confirmed_US_df %>%

  # Remove the first five columns, which are universal geolocation identifiers
  select(-c(1:5)) %>%

  # Remove the combined string of county, province and country
  select(-Combined_Key)


# RENAME COLUMNS ---------------------------------------------------------------
ts_confirmed_US_df <-
  ts_confirmed_US_df %>%

  # Rename the county
  rename("administration" = "Admin2") %>%

  # Rename the province
  rename("province" = "Province_State") %>%

  # Rename the country
  rename("country" = "Country_Region") %>%

  # Rename the Lat and Long variables to lower-case, as good practice
  rename('lat' = 'Lat') %>%
  rename('long' = 'Long_')


# TIDY the dataset into LONG FORMAT --------------------------------------------
ts_confirmed_US_df <-
  ts_confirmed_US_df %>%
  pivot_longer(-c(1:5),
               names_to = 'date_observation',
               values_to = 'cases')


# CONVERT DATES from chr to date -----------------------------------------------
ts_confirmed_US_df <-
  ts_confirmed_US_df %>%
  mutate(date_observation = mdy(date_observation))



# Wrangle data from 'ts_deaths_world_df': time_series_covid_19_deaths.csv ------
print('The structure of ts_deaths_world_df is: ')
str(ts_deaths_world_df)


# RENAME COLUMNS ---------------------------------------------------------------
ts_deaths_world_df <-
  ts_deaths_world_df %>%

  # Rename the province
  rename("province" = `Province/State`) %>%

  # Rename the country
  rename("country" = `Country/Region`) %>%

  # Rename the Lat and Long variables to lower-case, as good practice
  rename('lat' = 'Lat') %>%
  rename('long' = 'Long')


# Fix values in PROVINCE column ------------------------------------------------
ts_deaths_world_df <-
  ts_deaths_world_df %>%
  mutate(province = str_replace_all(province, pattern = 'Shaanxi',
                                    replacement = 'Shanxi')) %>%
  filter(province %in% c('Diamond Princess', 'Grand Princess',
                         'MS Zaandam', 'Recovered') == FALSE) %>%
  na_if('Bonaire, Sint Eustatius and Saba')


# Fix values in COUNTRY column -------------------------------------------------
ts_deaths_world_df <-
  ts_deaths_world_df %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(country = str_replace_all(
    country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))',
    replacement = 'Congo')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(country = str_replace_all(
    country, pattern = 'Holy See',
    replacement = 'Vatican City')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Arab Emirates',
    replacement = 'UAE')) %>%

  # Convert 'United Kingdom' to 'UK', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Kingdom',
    replacement = 'UK')) %>%

  # Convert 'Cote d'Ivoire' to 'Ivory Coast' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Cote d\'Ivoire',
    replacement = 'Ivory Coast')) %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(
    country, pattern = 'Burma',
    replacement = 'Myanmar')) %>%

  # Convert 'Korea, South' to 'South Korea' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Korea), (South)',
    replacement = '\\2 \\1')) %>%

  # Convert 'Taiwan*' to 'Taiwan', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Taiwan)\\*',
    replacement = '\\1')) %>%

  # Convert 'Czechia' to 'Czech Republic', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Czechia',
    replacement = 'Czech Republic')) %>%

  # Remove entries related to 'Diamond Princess', 'Grand Princess', 'MS Zaandam'
  filter(country %in% c('Diamond Princess',
                        'Grand Princess', 'MS Zaandam') == FALSE) %>%

  # Properly adjust country for Macau, so that it is 'Macau'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Macau') ~ 'Macau',
                             TRUE ~ country)) %>%

  # # Properly adjust country for Hong Kong, so that it is 'Hong Kong'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Hong Kong') ~ 'Hong Kong',
                             TRUE ~ country))



# Transform MISSING VALUES in PROVINCE column ----------------------------------
# If the province is unknown, we just assign it to the name of the country
ts_deaths_world_df <-
  ts_deaths_world_df %>%
  mutate(province = if_else(is.na(province) == TRUE,
                            true = country,
                            false = province))


# TIDY the dataset into LONG FORMAT --------------------------------------------
ts_deaths_world_df <-
  ts_deaths_world_df %>%
  pivot_longer(-c(1:4),
               names_to = 'date_observation',
               values_to = 'cases')


# CONVERT DATES from chr to date -----------------------------------------------
ts_deaths_world_df <-
  ts_deaths_world_df %>%
  mutate(date_observation = mdy(date_observation))


# AGGREGATE CASES for Canada ---------------------------------------------------
# Canada is the only country in the dataset that has a problem,
# its values for 'confirmed' and 'deaths' are aggregated within provinces,
# however the values for 'recovered' are aggregated across the country
# Therefore, we will standardize the case aggregation to be across the country

canada_confirmed_subset <-
  ts_deaths_world_df %>%

  # Remove all provinces from Canada
  filter(country == 'Canada') %>%
  mutate(province = 'Canada') %>%

  # Summarize the total number of cases over the entire country
  group_by(date_observation) %>%
  mutate(cases = sum(cases)) %>%

  # Only keep 1 aggregated observation for every observation day
  distinct(date_observation, .keep_all = TRUE)


# Join the new wrangled data for Canada with the old dataframe,
# from which we have removed the observations related to Canada,
# after which we arrange ascendingly by country, then by province

ts_deaths_world_df <-
  ts_deaths_world_df %>%
  filter(country != 'Canada') %>%
  full_join(canada_confirmed_subset,
            by = c("province", "country", "lat", "long",
                   "date_observation", "cases")) %>%
  arrange(country, province)



# Wrangle data from 'ts_deaths_US_df': time_series_covid_19_deaths_US.csv ------
print('The structure of ts_deaths_US_df is: ')
str(ts_deaths_US_df)


# REMOVING UNNECESSARY COLUMNS -------------------------------------------------
ts_deaths_US_df <-
  ts_deaths_US_df %>%

  # Remove the first five columns, which are universal geolocation identifiers
  select(-c(1:5)) %>%

  # Remove the combined string of county, province and country
  select(-Combined_Key) %>%

  # Remove the county population, since we have a separate dataset for that
  select(-Population)


# RENAME COLUMNS ---------------------------------------------------------------
ts_deaths_US_df <-
  ts_deaths_US_df %>%

  # Rename the county
  rename("administration" = "Admin2") %>%

  # Rename the province
  rename("province" = "Province_State") %>%

  # Rename the country
  rename("country" = "Country_Region") %>%

  # Rename the Lat and Long variables to lower-case, as good practice
  rename('lat' = 'Lat') %>%
  rename('long' = 'Long_')


# TIDY the dataset into LONG FORMAT --------------------------------------------
ts_deaths_US_df <-
  ts_deaths_US_df %>%
  pivot_longer(-c(1:5),
               names_to = 'date_observation',
               values_to = 'cases')


# CONVERT DATES from chr to date -----------------------------------------------
ts_deaths_US_df <-
  ts_deaths_US_df %>%
  mutate(date_observation = mdy(date_observation))



# Wrangle DF 'ts_recovered_world_df': time_series_covid_19_recovered.csv -------
print('The structure of ts_recovered_world_df is: ')
str(ts_recovered_world_df)


# RENAME COLUMNS ---------------------------------------------------------------
ts_recovered_world_df <-
  ts_recovered_world_df %>%

  # Rename the province
  rename("province" = `Province/State`) %>%

  # Rename the country
  rename("country" = `Country/Region`) %>%

  # Rename the Lat and Long variables to lower-case, as good practice
  rename('lat' = 'Lat') %>%
  rename('long' = 'Long')


# Fix values in PROVINCE column ------------------------------------------------
ts_recovered_world_df <-
  ts_recovered_world_df %>%
  mutate(province = str_replace_all(
    province, pattern = 'Shaanxi', replacement = 'Shanxi')) %>%
  filter(province %in% c('Diamond Princess', 'Grand Princess',
                         'MS Zaandam', 'Recovered') == FALSE) %>%
  na_if('Bonaire, Sint Eustatius and Saba')


# Fix values in COUNTRY column -------------------------------------------------
ts_recovered_world_df <-
  ts_recovered_world_df %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(country = str_replace_all(
    country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))',
    replacement = 'Congo')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(country = str_replace_all(
    country, pattern = 'Holy See',
    replacement = 'Vatican City')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Arab Emirates',
    replacement = 'UAE')) %>%

  # Convert 'United Kingdom' to 'UK', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'United Kingdom',
    replacement = 'UK')) %>%

  # Convert 'Cote d'Ivoire' to 'Ivory Coast' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Cote d\'Ivoire',
    replacement = 'Ivory Coast')) %>%

  # Convert 'Burma' to 'Myanmar'
  mutate(country = str_replace_all(
    country, pattern = 'Burma',
    replacement = 'Myanmar')) %>%

  # Convert 'Korea, South' to 'South Korea' for consistency with other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Korea), (South)',
    replacement = '\\2 \\1')) %>%

  # Convert 'Taiwan*' to 'Taiwan', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = '(Taiwan)\\*',
    replacement = '\\1')) %>%

  # Convert 'Czechia' to 'Czech Republic', for consistency with the other datasets
  mutate(country = str_replace_all(
    country, pattern = 'Czechia',
    replacement = 'Czech Republic')) %>%

  # Remove entries related to 'Diamond Princess', 'Grand Princess', 'MS Zaandam'
  filter(country %in% c('Diamond Princess',
                        'Grand Princess', 'MS Zaandam') == FALSE) %>%

  # Properly adjust country for Macau, so that it is 'Macau'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Macau') ~ 'Macau',
                             TRUE ~ country)) %>%

  # # Properly adjust country for Hong Kong, so that it is 'Hong Kong'
  # and not 'China' (consistent with the other datasets)
  mutate(country = case_when((province == 'Hong Kong') ~ 'Hong Kong',
                             TRUE ~ country))


# Transform MISSING VALUES in PROVINCE column ----------------------------------
# If the province is unknown, we just assign it to the name of the country
ts_recovered_world_df <-
  ts_recovered_world_df %>%
  mutate(province = if_else(is.na(province) == TRUE,
                            true = country,
                            false = province))


# TIDY the dataset into LONG FORMAT --------------------------------------------
ts_recovered_world_df <-
  ts_recovered_world_df %>%
  pivot_longer(-c(1:4),
               names_to = 'date_observation',
               values_to = 'cases')

ts_recovered_world_df <-
  ts_recovered_world_df %>%
  mutate(date_observation = mdy(date_observation))


# AGGREGATE CASES for Canada ---------------------------------------------------
# Canada is the only country in the dataset that has a problem,
# its values for 'confirmed' and 'deaths' are aggregated within provinces,
# however the values for 'recovered' are aggregated across the country
# Therefore, we will standardize the case aggregation to be across the country

canada_confirmed_subset <-
  ts_recovered_world_df %>%

  # Remove all provinces from Canada
  filter(country == 'Canada') %>%
  mutate(province = 'Canada') %>%
  mutate(lat = 53.9333) %>%
  mutate(long = -116.5765) %>%

  # Summarize the total number of cases over the entire country
  group_by(date_observation) %>%
  mutate(cases = sum(cases)) %>%

  # Only keep 1 aggregated observation for every observation day
  distinct(date_observation, .keep_all = TRUE)


# Join the new wrangled data for Canada with the old dataframe,
# from which we have removed the observations related to Canada,
# after which we arrange ascendingly by country, then by province

ts_recovered_world_df <-
  ts_recovered_world_df %>%
  filter(country != 'Canada') %>%
  full_join(canada_confirmed_subset,
            by = c("province", "country", "lat", "long",
                   "date_observation", "cases")) %>%
  arrange(country, province)



# Wrangle DF  'population_by_country_df': wpp2019_total_population.csv ---------
population_by_country_df <-
  population_by_country_df %>%
  filter(country %in% daily_covid_trends_df$country) %>%
  mutate(total_population = as.double(total_population)) %>%
  mutate(population_density = as.double(population_density))



# Write data -------------------------------------------------------------------
write_csv(x = daily_covid_trends_df,
          path = ".//data//_clean//daily_covid_trends_df_clean.csv")
write_csv(x = patient_data_first_df,
          path = ".//data//_clean//patient_data_first_df_clean.csv")
write_csv(x = patient_data_second_df,
          path = ".//data//_clean//patient_data_second_df_clean.csv")
write_csv(x = ts_confirmed_world_df,
          path = ".//data//_clean//ts_confirmed_world_df_clean.csv")
write_csv(x = ts_confirmed_US_df,
          path = ".//data//_clean//ts_confirmed_US_df_clean.csv")
write_csv(x = ts_deaths_world_df,
          path = ".//data//_clean//ts_deaths_world_df_clean.csv")
write_csv(x = ts_deaths_US_df,
          path = ".//data//_clean//ts_deaths_US_df_clean.csv")
write_csv(x = ts_recovered_world_df,
          path = ".//data//_clean//ts_recovered_world_df_clean.csv")
write_csv(x = population_by_country_df,
          path = ".//data//_clean//population_by_country_df_clean.csv")
