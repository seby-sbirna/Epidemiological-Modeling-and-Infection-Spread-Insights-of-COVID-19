# Clear workspace --------------------------------------------------------------
rm(list = ls())


# Load libraries ---------------------------------------------------------------
library("tidyverse")
library("deSolve")


# Define functions -------------------------------------------------------------
source(file = "R/99_func.R")


# Load data --------------------------------------------------------------------
df_ts <-
  read_csv(file = "data/_augmented/final_ts_world_df_augm.csv",
           col_types = cols())

df_SIR <-
  read_csv(file = "data/_augmented/SIR_df.csv",
           col_types = cols())


# 1. Plot Scandinavia time series ----------------------------------------------
p_scandinavia_obs <-
  ggplot(data = df_ts %>%
            filter(region == 'Denmark'|region == 'Norway'|region == 'Sweden'),
         mapping = aes(x = days_since_first,
                       y = total_confirmed,
                       group = region,
                       color = region)) +
  labs(title = "COVID-19 in Scandinavia",
       subtitle = "Infections by country",
       x = "Days since first infection", y = "Total infections (cumulative)",
       color = "Country") +
  geom_point() +
  xlim(c(0,100))

ggsave(path = "results",
       filename = "04_scandinavia_obs.png",
       plot = p_scandinavia_obs,
       width = 6,
       height = 4)


# 2. Plot observed number of infected/recovered in Denmark ---------------------
# First convert df to long format for plotting
df_SIR_long <-
  df_SIR %>%
  select(region, date_observation, days_since_first, I, R) %>%
  filter(days_since_first>=0) %>%
  pivot_longer(cols = c(-region,-date_observation,-days_since_first),
               names_to = "variable",
               values_to = "value")

p_denmark_obs <-
  ggplot(data = df_SIR_long %>% filter(region == "Denmark"),
         mapping = aes(x = date_observation,
                       y = value,
                       group = variable,
                       color = variable)) +
  labs(title = "COVID-19 in Denmark",
       subtitle = "Infected and recovered pateients",
       x = "Date", y = "Number of cases", color = "Cases") +
  scale_color_manual(labels = c("Infected", "Recovered"),
                     values = c("#F8766D", "#619CFF")) +
  geom_point()


ggsave(path = "results",
       filename = "04_denmark_obs.png",
       plot = p_denmark_obs,
       width = 6,
       height = 4)


# 3. SIR modelling - Sweden ----------------------------------------------------
# ODEs describing susceptible, infected and recovered
SIR <- function(time,state,parameters) {
  with(as.list(c(state,parameters)), {
  dS = -beta*I*S / N
  dI = beta*I*S / N - gamma*I
  dR = gamma*I
  #dD = mu*I
  return(list(c(dS,dI,dR)))
  })
}

# vector of time points from point 0 to latest
max_days <-
  df_SIR %>%
  filter(region == 'Sweden') %>%
  select(days_since_first) %>%
  max()

times <- seq(0,max_days)

# population size
N <-
  df_SIR %>%
  filter(region == 'Sweden') %>%
  select(N) %>%
  min()

# initial values
initial_values <- c(
  S = N - 1,
  I = 1,
  R = 0
)

# function to be minimized (residual square error)
RSS <- function(parameters) {
  #predicted infections
  fit = ode(y = initial_values,
            times = times,
            func = SIR,
            parms = parameters)[, 3]
  #observed infections
  obs = df_SIR %>%
    filter(region == "Sweden" & days_since_first >= 0) %>%
    select(I)
  return(sum((fit - obs)^2))
}


#optimize parameters
opt <-
  optim(c(beta=0.5, gamma=0.5),
        RSS,
        method = "L-BFGS-B",
        lower = c(0, 0),
        upper = c(1, 1))

#opt$par

# calculate the reproductive rate R0
R0 <- opt$par[1]/opt$par[2]


# Plot the result --------------------------------------------------------------
times <- seq (0,150)


df_fitted <-
  ode(y = initial_values,
      times = times,
      func = SIR,
      parms = opt$par) %>%
  data.frame() %>%
  as_tibble()


df_Sweden_long <-
  df_SIR %>%
  filter(region == "Sweden" & days_since_first >= 0) %>%

  #join the observed and fitted values in one df
  full_join(df_fitted,
            by = c("days_since_first" = "time"),
            suffix = c("_observed","_fitted")) %>%

  #long format for plotting
  select(days_since_first,
         I_observed, I_fitted,
         R_observed, R_fitted) %>%

  pivot_longer(cols = -days_since_first,
               names_to = "measure",
               values_to = "value")


# compare observed and predicted infections
p_sweden_obs_fit <-
  ggplot(df_Sweden_long %>%
            filter(measure == "I_observed" | measure == "I_fitted"),
         mapping = aes(x = days_since_first,
                       y = value,
                       group = measure,
                       color = measure)) +
  geom_point() +
  xlim(c(0,max_days)) +
  ylim(c(0,30000)) +
  labs(title = "Sweden COVID-19: Active cases",
       subtitle = "Observed vs predicted",
       x = "Days since first infection",
       y = "Active infections",
       color = "Measure") +
  scale_color_manual(labels = c("Predicted", "Observed"),
                     values = c("#F8766D", "#619CFF"))


ggsave(path = "results",
       "04_sweden_obs_vs_fit.png",
       p_sweden_obs_fit,
       width = 6,
       height = 4
       )


# plot the prediction on a longer time scale
p_sweden_pred <-
  ggplot(df_Sweden_long %>%
            filter(measure == "I_fitted" | measure == "R_fitted"),
         mapping = aes(x = days_since_first,
                       y = value,
                       group = measure,
                       color = measure)) +
  geom_point() +
  xlim(c(0,150)) +
  labs(title = "Sweden COVID-19: SIR-based prediction",
       x = "Days since first infection",
       y = "Number of cases",
       color = "Measure") +
  scale_color_manual(labels = c("Infected", "Recovered"),
                     values = c("#F8766D", "#619CFF"))


ggsave(path = "results",
       "04_sweden_pred.png",
       p_sweden_pred,
       width = 6,
       height = 4
)
