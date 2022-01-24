# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
# library(broom)

#define constants
YMAX_DEFAULT <- 160

#read neighborhood defaults
config <- read_csv("config/config.csv") %>%
              mutate(year = factor(year),
                     quorum = numberhomes %/% 4)

# read data file
votes <-
    list.files(path = "sources/", pattern = "*.csv", full.names = TRUE) %>%
    map_df(~read_csv(.)) %>%
                    mutate(date = as.Date(date, format = "%Y-%m-%d"),
                           year = factor(year(date))) %>%
                     inner_join(config) %>%
                     mutate(votesneeded = quorum - votesreceived,
                            daysuntilelection = meetingdate - date,
                            pastquorum = ifelse(votesreceived >= quorum,
                                                TRUE, 
                                                FALSE)
                            )

year_range <-
    votes %>%
    distinct(year) %>%
    pull(year)

year_now <- last(year_range)
# comp_range2 <- year_range[1:length(year_range) - 1]
comp_range <- year_range[seq_len(length(year_range) - 1)]

sub_text <- paste0(first(comp_range), "-", last(comp_range))

color_range <- c(rep("gray50", length(year_range) - 1), "red")
line_type_range <- c(rep("dotted", length(year_range) - 1), "solid")
line_size_range <- c(rep(0.5, length(year_range) - 1), 1.5)

votes %>%
    mutate(count = case_when(year == year_now ~ votesreceived
                             )
            ) %>%
    ggplot +
    aes(x = daysuntilelection,
        y = votesreceived,
        label = count,
        color = year,
        linetype = year,
        size = year) +
    geom_line() +
    geom_point(data = votes %>% filter(year == year_now),
               aes(label = NA),
               color = "red",
               size = 3)  +
    geom_label(size = 2) +
    scale_x_reverse(breaks = seq(0, 49, 7)) +
    scale_y_continuous(breaks = seq(0, 200, 20)) +
    labs(x = "Days until the Annual Meeting",
         y = "Votes received",
         title = "Comparison of incoming votes by year",
         subtitle = paste0("Comparing ",
                            year_now,
                            " (in <b style = \"color:red\"\">red</b>) to ",
                            sub_text,
                            " (in <b style = \"color:dimgray\"\">gray</b>)"),
         caption = "\U00A9 2022, Glenlake Upstate Homeowners Assocation."
        ) +
    geom_hline(yintercept = 120, color = "gray50", lty = 2) +
    geom_vline(xintercept = 0, color = "gray50", lty = 2) +
    scale_color_manual(values = color_range) +
    scale_linetype_manual(values = line_type_range) +
    scale_size_manual(values = line_size_range) +
    annotate("label",
             x = 29,
             y = 120,
             label = "Votes needed to meet quorum",
             size = 3,
            #  color = "black",
             hjust = 0) +
    geom_richtext(x = 0,
             y = 10,
             label = "Annual Meeting",
             hjust = 0,
             angle = 90,
             size = 3,
             color = "black"
             ) +
    theme_light() +
    theme(legend.position = "none", 
          plot.subtitle = element_markdown())

ggsave("trends/vote-count-comparison.png", width = 6, height = 6)