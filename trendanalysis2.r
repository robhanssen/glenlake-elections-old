# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(broom)
library(ggtext)
library(patchwork)
options(bitmapType = "cairo")
theme_set(theme_light())

days60 = 120/60

#read neighborhood defaults
config <- read_csv("config/config.csv") %>%
              mutate(year = factor(year),
                     quorum = numberhomes %/% 4)

# select all years to not be included in the baseline range
analysis_years <- c(2022)

# read data file
votes <-
    list.files(path = "sources/", pattern = "*.csv", full.names = TRUE) %>%
    map_df(~read_csv(.)) %>%
                    mutate(date = as.Date(date, format = "%Y-%m-%d"),
                           year = factor(year(date))) %>%
                     inner_join(config) %>%
                     mutate(votesneeded = quorum - votesreceived,
                            daysuntilelection = as.numeric(meetingdate - date),
                            pastquorum = ifelse(votesreceived >= quorum,
                                                TRUE,
                                                FALSE)
                            )

full_data_set <- votes

votes <- votes %>% filter(!year %in% analysis_years)
votes_this_year <- full_data_set %>% filter(year %in% analysis_years)


votemodel <- function(tbl) {
    lm(votesneeded ~ daysuntilelection, data = tbl)
}

models <-
    votes %>%
    group_by(year) %>%
    nest() %>%
    mutate(votemodel = map(data, votemodel))

modeldata <-
    models %>%
    mutate(modeldata = map(votemodel, augment)) %>%
    unnest(modeldata)

modelparameters <-
    models %>%
    mutate(modelparameters = map(votemodel, tidy)) %>%
    unnest(modelparameters) %>% 
    filter(term == "daysuntilelection") %>%
    mutate(df = map_dbl(data, nrow) - 2,
            t_alpha = qt(0.025, df, lower.tail = FALSE), 
            err.bar = ifelse(df < 4, 0, t_alpha * std.error)) %>%
    select(-data, -votemodel, -term)

modelinfo <-
    models %>%
    mutate(modelinfo = map(votemodel, glance)) %>%
    unnest(modelinfo) %>%
    select(-data, -votemodel) %>% 
    inner_join(modelparameters, by = "year") %>% 
    mutate(seer = estimate * sqrt((1/r.squared -1 )/df.y) * t_alpha, 
           seer = ifelse(df.y < 4, 0, seer))

modelofmodel <-
    modelparameters %>% 
        mutate(year = as.numeric(paste(year))) %>%
        lm(estimate ~ year, data = .) %>%
    augment(., newdata = tibble(year = 2018:2023)) %>%
    mutate(daysrequired = ceiling(120 / .fitted))

modelinfo %>%
    ggplot +
        aes(x = year, y = estimate) +
        geom_point(color = "darkgreen", alpha = 0.5, size = 3) +
        geom_col(alpha = 0.5, fill = "darkgreen") +
        expand_limits(y = 0.0) +
        scale_y_continuous(breaks = 2 * -10:20) +
        labs(x = "Year",
             y = "Average votes per day",
             title = "Incoming votes per day over the last election years",
             subtitle = "Vote intake has been dropping every year",
             caption = "Actual data in green;\nModels and predictions in red") +
        geom_errorbar(aes(ymax = estimate + err.bar, ymin = estimate - err.bar), width=.2) +
        geom_line(data = modelofmodel,
                  aes(x = factor(year), y = .fitted, group = TRUE),
                  color = "red",
                  lty = "dotdash",
                  size = 1) +
        geom_point(data = modelofmodel %>% filter(year > 2021),
                  aes(x = factor(year), y = .fitted, group = TRUE),
                  color = "red",
                  size = 3) +
        geom_col(data = modelofmodel  %>% filter(year > 2021),
                  aes(x = factor(year), y = .fitted, group = TRUE),
                  fill = "red",
                  alpha = 0.5,
                  size = 3) +
        geom_hline(yintercept = days60,
                   lty = 1,
                   alpha = 0.3,
                   color = "red",
                   size = 1) +
        annotate("label",
                 x = first(factor(modelofmodel$year)),
                 hjust = 0.05,
                 y = days60, #days28 * 1.17,
                 color = "white",
                 label.size = NA,
                 fill = "red",
                 label = "Minimum rate mandated by the CC&R (60 days maximum notice)") +
        geom_label(data = modelofmodel,
                  aes(factor(year), 0.5, label = daysrequired),
                  color = "white",
                  fill = "darkgreen",
                  alpha = .6,
                  label.size = NA,
                  ) +
        annotate("label",
                 x = first(factor(modelofmodel$year)),
                 hjust = .1,
                 y = 1,
                 color = "white",
                 fill = "darkgreen",
                 alpha = .6,
                 label.size = NA,
                 label = "Minimum required days of election season")
