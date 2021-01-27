# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
options(bitmapType='cairo')

#define constants
MEETINGDATE = as.Date("2021-02-06", format="%Y-%m-%d")
NUMBEROFHOMES = 482
QUORUM = NUMBEROFHOMES %/% 4  # 25% quorum requirement
YMAX_DEFAULT = 160
SCALING = YMAX_DEFAULT / QUORUM
YLABELS = seq(0,NUMBEROFHOMES,30)
XLIMITS = c(MEETINGDATE - months(1),MEETINGDATE + days(3))
PERCENTBREAKS = seq(0,4*QUORUM,25)
YEAR = 2021

# read data file
votes <-
    list.files(path="sources/", pattern = "*.csv") %>% 
    map_df(~read_csv(paste0("sources/",.))) %>%
                    mutate(date = as.Date(date, format="%Y-%m-%d"),
                           year = factor(year(date)),
                           displaydate = as.Date(paste0(year(today()),"-",month(date),"-",day(date))),                            
                           pastquorum = ifelse(votesreceived < QUORUM, FALSE, TRUE)
                            )

# caption generator
lastgen = format(today(), format="%b %d, %Y")
lastupdate = format(max(votes$date), format="%b %d, %Y")
capt = paste0("\U00A9 ", YEAR,", Glenlake Homeowners Association\nLast updated: ", lastgen, "\nLast data entry: ", lastupdate)

# y-axis max
votesmax = max(votes$votesreceived)
ymax = ifelse(votesmax < QUORUM, 
                    YMAX_DEFAULT, 
                    ((votesmax * SCALING) %/% 10 + 1) * 10
                    )

#define YLIMIT constants
YLIMITS = c(0 , ymax)


votes %>% ggplot + aes(x=displaydate, y=votesreceived, color=year, label=votesreceived) + 
            #geom_smooth(method="lm", lty=2, color="gray") + 
            geom_point() + 
            geom_line() + 
            scale_x_date(date_breaks="1 week", date_labels = "%b %d", limit=XLIMITS) + 
            scale_y_continuous(limit = YLIMITS, breaks = YLABELS, 
                               sec.axis = sec_axis(~ ./QUORUM*100, breaks=PERCENTBREAKS, name="Quorum (%)")
                                ) + 
            labs(x="Date", y="Votes received", caption=capt, color="Year", fill="Pass quorum?")  +
            geom_hline(yintercept = QUORUM, lty=2, color="red") + 
            geom_vline(xintercept = MEETINGDATE, lty = 2, color = "red") + 
            geom_label_repel(aes(displaydate, votesreceived, label = votesreceived, fill = pastquorum), color="white") +             
            annotate("text", x = MEETINGDATE - days(28), y = QUORUM*1.05, label = paste0("Quorum: ", QUORUM)) + 
            annotate("text", x = MEETINGDATE - days(1), y = QUORUM %/% 2, label = "Annual Meeting", angle = 90) + 
            theme_light() #+ theme(legend.position = "none")

ggsave("graphs/vote-tracking-comparison.png")
ggsave("graphs/vote-tracking-comparison.pdf")
