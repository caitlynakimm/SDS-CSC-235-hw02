---
title: "Homework 1 Part 1: Data Visualizations"
author: "Caitlyn Kim"
format: 
  pdf:
  fig-pos: "H"
code-overflow: wrap
editor: source
---

```{r}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(RColorBrewer)
```

```{r}
ems <- read.csv("ems_incident_dispatch.csv")
```

```{r}
##Summary Statistics

#quantitative vars - incident response time and incident travel time
ems_cleaned <- ems |> 
    filter(INCIDENT_RESPONSE_SECONDS_QY != "") |> 
    mutate(response_time = round(as.numeric(INCIDENT_RESPONSE_SECONDS_QY) /60, 2),
            travel_time = round(as.numeric(INCIDENT_TRAVEL_TM_SECONDS_QY) /60, 2)) |>
            select(-INCIDENT_RESPONSE_SECONDS_QY, -INCIDENT_TRAVEL_TM_SECONDS_QY)

quant_vars_summary <- ems_cleaned |>
    summarize(mean_response = mean(response_time, na.rm = TRUE),
            sd_response = sd(response_time, na.rm = TRUE),
            median_response = median(response_time, na.rm = TRUE),
            mean_travel = mean(travel_time, na.rm = TRUE),
            sd_travel = sd(travel_time, na.rm = TRUE),
            median_travel = median(travel_time, na.rm = TRUE))

#ordinal vars - initial severity level and final severity level
initial_sev_freq <- ems_cleaned |> count(INITIAL_SEVERITY_LEVEL_CODE)
initial_sev_mode <- initial_sev_freq |> filter(n == max(n))

final_sev_freq <- ems_cleaned |> count(FINAL_SEVERITY_LEVEL_CODE)
final_sev_mode <- final_sev_freq |> filter(n == max(n))

#categorical vars - final call type and borough
final_call_freq <- ems_cleaned |> count(FINAL_CALL_TYPE)
final_call_mode <- final_call_freq |> filter(n == max(n))

borough_freq <- ems_cleaned |> count(BOROUGH)
borough_mode <- borough_freq |> filter(n == max(n))
```

```{r}
##Distribution Visualizations

#quantitative variable visualizations
ggplot(ems_cleaned, aes(x = response_time)) + 
  geom_histogram(bins = 20, boundary = 0) +
  theme_minimal() +
  labs(title = "Distribution of EMS Incident Response Time in New York (2025)",
       x = "Response Time (minutes)",
       y = "Frequency")

ggplot(ems_cleaned, aes(x = travel_time)) + 
  geom_histogram(bins = 20, boundary = 0) +
  theme_minimal() +
  labs(title = "Distribution of EMS Incident Travel Time in New York (2025)",
       x = "Travel Time (minutes)",
       y = "Frequency")


#ordinal variable visualizations
ggplot(initial_sev_freq, aes(x = factor(INITIAL_SEVERITY_LEVEL_CODE), y = n)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Comparison of Initial Severity Levels Reported in New York (2025)",
       x = "Initial Reported Severity Level",
       y = "Frequency")

ggplot(final_sev_freq, aes(x = factor(FINAL_SEVERITY_LEVEL_CODE), y = n)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Comparison of Final Severity Levels Reported in New York (2025)",
       x = "Final Reported Severity Level",
       y = "Frequency")


#categorial variable visualizations
top_20_calls_freq <- final_call_freq |>
  slice_max(n, n = 20)

ggplot(top_20_calls_freq, aes(x = reorder(FINAL_CALL_TYPE, n), y = n)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Comparison of Final Call Types in New York (2025)",
       x = "Final Call Type",
       y = "Frequency")

ggplot(borough_freq, aes(x = BOROUGH, y = n)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Comparison of Incidents by Borough in New York (2025)",
       x = "Borough",
       y = "Frequency")
```

```{r}
##Relationships

#relationship 1 - travel time by borough
ggplot(ems_cleaned, aes(x = travel_time)) + 
  geom_histogram(bins = 20, boundary = 0) +
  facet_wrap(~ BOROUGH) +
  theme_minimal() +
  labs(title = "Distribution of EMS Incident Travel Time by Borough in New York (2025)",
       x = "Travel Time (minutes)",
       y = "Frequency")


#relationship 2 - initial vs. final severity levels
sev_level_compare <- ems_cleaned |>
  select(INITIAL_SEVERITY_LEVEL_CODE, FINAL_SEVERITY_LEVEL_CODE) |>
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "sev_level") |>
  mutate(type = ifelse(type == "INITIAL_SEVERITY_LEVEL_CODE", "Initial", "Final"),
         type = factor(type, levels = c("Initial", "Final"))) |>
  count(type, sev_level)

ggplot(sev_level_compare, aes(x = factor(sev_level), y = n, fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Initial vs. Final Severity Levels Reported in New York (2025)",
       x = "Severity Level",
       y = "Frequency",
       fill = "Severity Type") +
  scale_fill_brewer(palette = "Set1")


#relationship 3 - final call type by borough
top_20_calls <- top_20_calls_freq |> pull(FINAL_CALL_TYPE)

ems_cleaned |> 
  filter(FINAL_CALL_TYPE %in% top_20_calls) |>
ggplot(aes(x = FINAL_CALL_TYPE, fill = BOROUGH)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Final Call Types by Borough in New York (2025)",
       x = "Final Call Type",
       y = "Frequency") +
  scale_fill_brewer(palette = "Dark2")
```


```{r}
##Hypotheses
#1: In 2025, it took most often 5-10 minutes for EMS to arrive at the incident location in New York.

#2: In 2025, the most common initial and final severity level of incidents in New York was 5.

#3: In 2025, the top call type for incidents in New York was sickness with the most cases in the Bronx, Brooklyn, Manhattan, and Queens.
```

