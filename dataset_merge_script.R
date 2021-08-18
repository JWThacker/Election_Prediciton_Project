library(readr);
library(magrittr);
library(dplyr);
library(ggplot2);
library(ggpubr);
library(mvoutlier);
library(stringr);
library(keras);
library(patchwork);
setwd("/Users/jaredthacker/STAT_Courses/multi_vars/project");
df <- read_table('http://users.stat.ufl.edu/~winner/data/clinton1.dat',
                 col_names=FALSE);
web_scraped_table <- read_csv("web_scraped_table.csv", col_names=T)
labels <- c("county_name", "percent_for_clinton", "median_age",
           "mean_savings", "per_capita_income", "percent_in_poverty",
           "percent_veterans", "percent_female", "population_density",
           "percent_in_nursing_homes", "crime_index_PC");
colnames(df) <- labels
head(df);

df <- df %>% dplyr::mutate(state=str_split_fixed(unlist(df["county_name"]),
                                    pattern=",", n=2)[,2]) %>%
  mutate(county_name=str_trim(str_split_fixed(unlist(df["county_name"]),
                                              pattern=",", n=2)[,1]))

df <- df %>% mutate(state=str_trim(unlist(df["state"])))

df[df["county_name"] == "St. John the Baptist","state"]

sum(df["state"] == "")

df[df["county_name"] == "St. John the Baptist", "state"] = "LA";

df[df["county_name"] == "District of Columbia", "state"] = "D.C.";

states_alphabetical <- str_sort(unlist(unique(df[,"state"])));
df <- df %>% dplyr::mutate(state=str_replace_all(unlist(df[,"state"]),
                                                 pattern=c("^M$" = "MN")));
states_alphabetical <- str_sort(unlist(unique(df[,"state"])));
states_alphabetical;

state_color <- c("AL" = "Bush", "AR" = "Clinton", "AZ" = "Bush", "CA" = "Clinton", "CO" = "Clinton", "CT" = "Clinton", "D.C."="Clinton",
  "DE" = "Clinton", "FL" = "Bush", "GA" = "Clinton", "IA" = "Clinton", "ID" = "Bush", "IL" = "Clinton", "IN" = "Bush",
  "KS" = "Bush", "KY" = "Clinton", "LA" = "Clinton", "MA" = "Clinton", "MD" = "Clinton", "ME" = "Clinton", "MI" = "Clinton",
  "MN" = "Clinton", "MO" = "Clinton", "MS" = "Bush", "MT" = "Clinton", "NC" = "Bush", "ND" = "Bush", "NE" = "Bush",
  "NH" = "Clinton", "NJ" = "Clinton", "NM" = "Clinton", "NV" = "Clinton", "NY" = "Clinton", "OH" = "Clinton", "OK" = "Bush",
  "OR" = "Clinton", "PA" = "Clinton", "RI"= "Clinton", "SC" = "Bush", "SD" = "Bush", "TN" = "Clinton", "TX" = "Bush",
  "UT" = "Bush", "VA" = "Bush", "VT" = "Clinton", "WA" = "Clinton", "WI" = "Clinton", "WV" = "Clinton", "WY" = "Bush");

state_region <- c("AL" = "Southeast", "AR" = "Southeast", "AZ" = "West", "CA" = "West", "CO" = "West", "CT" = "Northeast", "D.C."="U.S Capital",
                  "DE" = "Southeast", "FL" = "Southeast", "GA" = "Southeast", "IA" = "Midwest", "ID" = "West", "IL" = "Northeast", "IN" = "Northeast",
                  "KS" = "Midwest", "KY" = "Southeast", "LA" = "Southeast", "MA" = "Northeast", "MD" = "Northeast", "ME" = "Northeast", "MI" = "Northeast",
                  "MN" = "Midwest", "MO" = "Midwest", "MS" = "Southeast", "MT" = "West", "NC" = "Southeast", "ND" = "Midwest", "NE" = "Midwest",
                  "NH" = "Northeast", "NJ" = "Northeast", "NM" = "West", "NV" = "West", "NY" = "Northeast", "OH" = "Northeast", "OK" = "Southeast",
                  "OR" = "West", "PA" = "Northeast", "RI"= "Northeast", "SC" = "Southeast", "SD" = "Midwest", "TN" = "Southeast", "TX" = "Southeast",
                  "UT" = "West", "VA" = "Southeast", "VT" = "Northeast", "WA" = "West", "WI" = "Northeast", "WV" = "Southeast", "WY" = "West");
df <- df %>% dplyr::mutate(color=str_replace_all(unlist(df[,"state"]),
                                                 pattern=state_color));

df <- df %>% dplyr::mutate(region=str_replace_all(unlist(df[,"state"]),
                                                   pattern=state_region));
length(state_region)
for_clint_by_state <- df %>% dplyr::group_by(state, color, region) %>%
  dplyr::summarise(mean_for_clint=mean(percent_for_clinton));



full_dataset <- inner_join(df, web_scraped_table)
sum(is.na(full_dataset))
write_csv(full_dataset, "full_dataset.csv");
