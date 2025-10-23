library(openxlsx)
library (readxl)
library (gtsummary)
library(tidyverse)
library(dplyr)
library(gt)
library (xlsx)
library(DescTools)
library(tiff)
library(webshot2)
library(psych)
library(rjags)
library(tibble)
library(knitr) 
library(rstan)
library(here)
library(bayesmeta)
library(brms)
library(bayestestR)
library(shinystan)

## Data input 
tdat <- read_excel(here("Data", "NCC_among_PWH_raw.xlsx"))                  

## Working data set
df <- tdat %>% 
  filter(specific_manif_table == 1)

## Data exploration
glimpse(df)
head(df)
summary(df)

## Cleaning data 
df <- df %>% 
  mutate(study_setting = case_when(
    study_setting == "Clinic_or_hospital" ~ "Health center",
    TRUE ~ study_setting
  ))

## Stan model
model_code <- "
data{
int<lower=0> N_studies;
int<lower=0> n[N_studies];
int<lower=0> y[N_studies];
int<lower=0> alpha_prior;
int<lower=0> beta_prior;
}

parameters {
vector <lower=0, upper=1> [N_studies]p;
}

model{
p ~ beta(alpha_prior,beta_prior);

for (i in 1:N_studies){
y[i] ~ binomial(n[i],p[i]);
}
}
        

generated quantities{
vector[N_studies] y_rep;
for (i in 1:N_studies){
y_rep[i] = binomial_rng (n[i],p[i]);
}
}
"
compiled_model <- stan_model(model_code = model_code)

## Data preperation
df <- df %>% 
  mutate(st_id = paste0("St_",row_number(df)))

df1 <- df %>% 
  select(st_id,n,y)


model_data <- list(
  N_studies = nrow(df),
  n = df$n,
  y = df$y,
  alpha_prior = 1,
  beta_prior =1
)

## Fit the model

fit <- sampling(
  compiled_model,
  data = model_data,
  iter = 10000,
  warmup = 5000,
  chains = 4,
  seed = 123
)

saveRDS(fit, file = here("Outputs", "Specific_manif_results.rds"))


x <- readRDS(here("Outputs","Specific_manif_results.rds"))

print(x, pars = "p", probs = c(0.025,0.50,0.975) )

## Model convergence checking
launch_shinystan(x)

## Create table

# Extract posterior samples
posterior_samples <- extract(x)

# Get summary statistics for theta parameters
theta_samples <- posterior_samples$p
colnames(theta_samples) <- df1$st_id

# Calculate posterior summaries
posterior_summaries <- data.frame(
  st_id = df1$st_id,
  y = df1$y,
  n = df1$n,
  Posterior_Mean = apply(theta_samples, 2, mean),
  Posterior_Median = apply(theta_samples, 2, median),
  CI_2.5 = apply(theta_samples, 2, quantile, probs = 0.025),
  CI_97.5 = apply(theta_samples, 2, quantile, probs = 0.975),
  SD = apply(theta_samples, 2, sd)
)


df2 <- df %>% 
  left_join(posterior_summaries, by = "st_id") %>% 
  mutate(Prop = paste0(
    sprintf("%.1f",Posterior_Median*100)," (", 
    sprintf("%.1f",CI_2.5*100)," - ", 
    sprintf("%.1f",CI_97.5*100),")")) %>% 
  select(id,first_author,manif,study_setting,Prop)

df2 <- df2 %>%
  mutate(manif = recode(manif, 
                        "tth" = "Tension-type headache",
                        "migraine" = "Migraine",
                        "migraine_&_tth" = "Migraine and Tension-type headache",
                        "Severe_headache" = "Severe headache",
                        "headache" = "Non-specific headache"))



table <- df2 %>%
  arrange(id) %>% 
  gt() %>% 
  cols_label(
    id = "Study ID",
    first_author = "Reference", 
    study_setting = "Setting",
    manif = "Manifestation",
    Prop = md("Bayesian estimate<br>%(95% CrI)")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>%
  fmt_number(
    columns = c(id),
    decimals = 0
  ) %>%
  tab_style(style = list(cell_fill(color = "lightgray")),
            locations = cells_body(rows = seq(1, nrow(df2), 2))) %>% 
  tab_options(
    table.font.size = px(12),
    table.font.names = "Times New Roman"
  )

table

## Export the table
table %>% 
  gtsave("Outputs/Table.docx")


#============================ THE END ==================================
