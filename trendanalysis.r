# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(broom)
library(ggtext)
options(bitmapType = "cairo")
theme_set(theme_light())

#read neighborhood defaults
config <- read_csv("config/config.csv") %>%
              mutate(year = factor(year),
                     quorum = numberhomes %/% 4)

find_value <- function(x, y, target = 0) {
    approx(y, x, xout = target)$y
}


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

votes <- votes %>% filter(!year %in% c(2022))

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
caption <- paste0("Based on data from ", min_year, " to ", max_year, "<BR>",
                  "R<sup>2</sup> = ", rsq, "<BR>",
                  "Average votes per day: ", slope, "<BR>",
                  "Minumum days of voting needed: ", ceiling(120 / slope)
                  )


votes %>%
    ggplot +
        aes(x = daysuntilelection, y = votesneeded, color = NULL) +
        geom_point() +
        scale_x_reverse(breaks = seq(0, 100, 7)) +
        expand_limits(x = 28) +
        geom_line(data = votesdata, aes(y = .fitted, color = NULL), lty = 2) +
        geom_vline(xintercept =  zero_days, lty = 2, color = "gray50") +
        geom_hline(yintercept = 0, color = "black") +
        geom_ribbon(aes(y = .fitted,
                        ymin = .lower,
                        ymax = .upper,
                        color = NULL),
                    fill = "lightblue",
                    alpha = 0.5,
                    data = votesdata) +
        labs(x = "Days until the Annual Meeting",
             y = "Votes still required to meet quorum",
             title = "Forecasting when the quorum will be met",
             subtitle = subtitle,
             caption = caption) +
        theme(
                plot.caption = element_markdown()
              )

ggsave("trends/quorum-forecast.pdf", width = 11, height = 8)