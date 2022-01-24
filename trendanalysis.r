# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(broom)
library(ggtext)
library(patchwork)
options(bitmapType = "cairo")
theme_set(theme_light())

#read neighborhood defaults
config <- read_csv("config/config.csv") %>%
              mutate(year = factor(year),
                     quorum = numberhomes %/% 4)

find_value <- function(x, y, target = 0) {
    approx(y, x, xout = target)$y
}

# select all years to not be included in the baseline range
analysis_years <- c(2023)

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

votesmodel <- with(votes,
                lm(votesneeded ~ daysuntilelection))


votesdata <- votesmodel %>%
                augment(interval = "confidence") %>%
                select(-votesneeded) %>%
                unique() %>%
                arrange(-daysuntilelection)


rsq <- votesmodel %>% glance() %>% pull(r.squared) %>% round(., 3)
lin_estimates <- votesmodel %>% tidy() %>% pull(estimate)

slope <- round(lin_estimates[2], 1)

min_day <- with(votesdata, find_value(daysuntilelection, .upper, 0))
max_day <- with(votesdata, find_value(daysuntilelection, .lower, 0))
zero_days <- c(min_day, max_day)
subtitle <- paste0("Quorum expected between ",
                  round(min_day, 0),
                  " and ",
                  round(max_day, 0),
                  " days before the Annual Meeting.\n"
                  )

min_year <- min(as.numeric(paste(votes$year)))
max_year <- max(as.numeric(paste(votes$year)))
caption <- paste0("Based on data from ", min_year, " to ", max_year, ", ",
                  "R<sup>2</sup> = ", rsq, "<BR>",
                  "Average votes per day: ", slope, "<BR>",
                  "Minumum days of voting needed: ", ceiling(120 / slope)
                  )


votesforecast <- votes %>%
    ggplot +
        aes(x = daysuntilelection, y = votesneeded, color = NULL) +
        geom_point() +
        scale_x_reverse(breaks = seq(0, 100, 7)) +
        expand_limits(x = 28) +
        geom_line(data = votesdata, aes(y = .fitted, color = NULL), lty = 2) +
        geom_vline(xintercept =  zero_days, lty = 3, color = "gray50") +
        geom_hline(yintercept = 0, color = "black") +
        geom_ribbon(aes(y = .fitted,
                        ymin = .lower,
                        ymax = .upper,
                        color = NULL),
                    fill = "lightblue",
                    alpha = 0.5,
                    data = votesdata) +
        annotate("label", x = 26, y = 0, label = "Quorum met") +
        labs(x = "Days until the Annual Meeting",
             y = "Votes still required to meet quorum",
             title = "Forecasting when the quorum will be met",
             subtitle = subtitle,
             caption = caption) +
        theme(
                plot.caption = element_markdown()
              )
              
votesforecast_thisyear <- votesforecast +
        geom_point(data = votes_this_year, color = "red") +
        geom_smooth(data = votes_this_year,
                    method = "lm",
                    se = FALSE,
                    color = "red",
                    lty = 2,
                    fullrange = TRUE)

ggsave("trends/quorum-forecast_thisyear.png",
       plot = votesforecast_thisyear,
       width = 11,
       height = 8)

ggsave("trends/quorum-forecast.png",
       plot = votesforecast,
       width = 6,
       height = 6)

#########################################

votes_per_day_by_year <- votes %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        averagevotesperday = coef(lm(votesneeded ~ daysuntilelection))[2]
    )

days60 <- 120 / 60
forecast_years <- 2
config$year <- as.numeric(paste(config$year))

fit_line <- lm(data = votes_per_day_by_year,
               averagevotesperday ~ as.numeric(paste(year))
               )  %>%
            augment(newdata = tibble(year = seq(min_year,
                                                max_year + forecast_years, 1
                                                )
                                    )
                    ) %>%
            left_join(config) %>%
            mutate(quorum = ifelse(!is.na(quorum), quorum, 120)) %>%
            mutate(year = factor(year),
                   required_length = ceiling(quorum / .fitted))

fit_col <- lm(data = votes_per_day_by_year,
               averagevotesperday ~ as.numeric(paste(year))
               )  %>%
            augment(newdata = tibble(year = seq(max_year + 1,
                                                max_year + forecast_years, 1
                                                )
                                    )
                    ) %>%
            mutate(year = factor(year))

votingrate <- votes_per_day_by_year %>%
    ggplot +
        aes(x = year, y = averagevotesperday) +
        geom_point(color = "darkgreen", alpha = 0.5, size = 3) +
        geom_col(alpha = 0.5, fill = "darkgreen") +
        expand_limits(y = 0.0) +
        scale_y_continuous(breaks = 2 * -10:20) +
        labs(x = "Year",
             y = "Average votes per day",
             title = "Incoming votes per day over the last election years",
             subtitle = "Vote intake has been dropping every year",
             caption = "Actual data in green;\nModels and predictions in red") +
        geom_line(data = fit_line,
                  aes(y = .fitted, group = TRUE),
                  color = "red",
                  lty = "dotdash",
                  size = 1) +
        geom_point(data = fit_col,
                  aes(y = .fitted, group = TRUE),
                  color = "red",
                  size = 3) +
        geom_col(data = fit_col,
                  aes(y = .fitted, group = TRUE),
                  fill = "red",
                  alpha = 0.5,
                  size = 3) +
        geom_hline(yintercept = days60,
                   lty = 1,
                   alpha = 0.3,
                   color = "red",
                   size = 1) +
        annotate("label",
                 x = first(fit_line$year),
                 hjust = 0.05,
                 y = days60, #days28 * 1.17,
                 color = "white",
                 label.size = NA,
                 fill = "red",
                 label = "Minimum rate mandated by the CC&R (60 days maximum notice)") +
        geom_label(data = fit_line,
                  aes(year, 0.5, label = required_length),
                  color = "white",
                  fill = "darkgreen",
                  alpha = .6,
                  label.size = NA,
                  ) +
        annotate("label",
                 x = first(fit_line$year),
                 hjust = .1,
                 y = 1,
                 color = "white",
                 fill = "darkgreen",
                 alpha = .6,
                 label.size = NA,
                 label = "Minimum required days of election season")

ggsave("trends/intake-votes-per-day.png",
       plot = votingrate,
       width = 6,
       height = 6)

(votesforecast / votingrate)
ggsave("trends/voteforecasts_combined.pdf", width = 8, height = 11)
ggsave("trends/voteforecasts_combined.png", width = 6, height = 10)

#######################################################

votes_per_day_by_year <- votes %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        averagevotesperday = coef(lm(votesneeded ~ daysuntilelection))[2],
        r.sq = summary(lm(votesneeded ~ daysuntilelection))$r.squared,
        df = n() - 2) %>%
    mutate(
        t_alpha = qt(0.10 / 2, df, lower.tail = FALSE),
        sterr = averagevotesperday * sqrt((1 / r.sq - 1) / df),
        err.bar = ifelse(df < 4, 0, t_alpha * sterr),
        slope_max = averagevotesperday + err.bar,
        slope_min = averagevotesperday - err.bar
    ) %>%
    select(-t_alpha, sterr, -r.sq, -df)

days60 <- 120 / 60
forecast_years <- 2
config$year <- as.numeric(paste(config$year))

fit_line <- lm(data = votes_per_day_by_year,
               averagevotesperday ~ as.numeric(paste(year))
               )  %>%
            augment(newdata = tibble(year = seq(min_year,
                                                max_year + forecast_years, 1
                                                )
                                    )
                    ) %>%
            left_join(config) %>%
            mutate(quorum = ifelse(!is.na(quorum), quorum, 120)) %>%
            mutate(year = factor(year),
                   required_length = ceiling(quorum / .fitted))

fit_col <- lm(data = votes_per_day_by_year,
               averagevotesperday ~ as.numeric(paste(year))
               )  %>%
            augment(newdata = tibble(year = seq(max_year + 1,
                                                max_year + forecast_years, 1
                                                )
                                    )
                    ) %>%
            mutate(year = factor(year))

votingrate <- votes_per_day_by_year %>%
    ggplot +
        aes(x = year, y = averagevotesperday) +
        geom_point(color = "darkgreen", alpha = 0.5, size = 3) +
        geom_col(alpha = 0.5, fill = "darkgreen") +
        expand_limits(y = 0.0) +
        scale_y_continuous(breaks = 2 * -10:20) +
        labs(x = "Year",
             y = "Average votes per day",
             title = "Incoming votes per day over the last election years",
             subtitle = "Vote intake has been dropping every year",
             caption = "Actual data in green;\nModels and predictions in red") +
        geom_errorbar(aes(ymax = slope_max, ymin = slope_min), width=.2) +
        geom_line(data = fit_line,
                  aes(y = .fitted, group = TRUE),
                  color = "red",
                  lty = "dotdash",
                  size = 1) +
        geom_point(data = fit_col,
                  aes(y = .fitted, group = TRUE),
                  color = "red",
                  size = 3) +
        geom_col(data = fit_col,
                  aes(y = .fitted, group = TRUE),
                  fill = "red",
                  alpha = 0.5,
                  size = 3) +
        geom_hline(yintercept = days60,
                   lty = 1,
                   alpha = 0.3,
                   color = "red",
                   size = 1) +
        annotate("label",
                 x = first(fit_line$year),
                 hjust = 0.05,
                 y = days60, #days28 * 1.17,
                 color = "white",
                 label.size = NA,
                 fill = "red",
                 label = "Minimum rate mandated by the CC&R (60 days maximum notice)") +
        geom_label(data = fit_line,
                  aes(year, 0.5, label = required_length),
                  color = "white",
                  fill = "darkgreen",
                  alpha = .6,
                  label.size = NA,
                  ) +
        annotate("label",
                 x = first(fit_line$year),
                 hjust = .1,
                 y = 1,
                 color = "white",
                 fill = "darkgreen",
                 alpha = .6,
                 label.size = NA,
                 label = "Minimum required days of election season")
