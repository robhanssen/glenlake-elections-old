# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
options(bitmapType='cairo')

#define constants
MEETINGDATE = as.Date("2020-02-08", format="%Y-%m-%d")
QUORUM = 120
YMAX_DEFAULT = 160
SCALING = YMAX_DEFAULT / QUORUM
YLABELS = seq(0,500,30)
XLIMITS = c(as.Date("2020-01-15"),as.Date("2020-02-13"))
PERCENTBREAKS = seq(0,4*QUORUM,25)
YEAR = 2020

# read data file
votes <- read_csv("sources/vote-tracking-2020.csv") %>% 
                    mutate(date = as.Date(date, format="%Y-%m-%d"),
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


votes %>% ggplot + aes(x=date, y=votesreceived, label=votesreceived) + 
            #geom_smooth(method="lm", lty=2, color="gray") + 
            geom_point() + 
            geom_line(lty=2, color="black") +
            scale_x_date(date_breaks="1 week", date_labels = "%b %d", limit=XLIMITS) + 
            scale_y_continuous(limit = YLIMITS, breaks = YLABELS, 
                               sec.axis = sec_axis(~ ./QUORUM*100, breaks=PERCENTBREAKS, name="Quorum (%)")
                                ) + 
            labs(x="Date", y="Votes received", caption=capt)  +
            geom_hline(yintercept = QUORUM, lty=2, color="red") + 
            geom_vline(xintercept = MEETINGDATE, lty = 2, color = "red") + 
            geom_label_repel(aes(date,votesreceived, label=votesreceived, fill=pastquorum), color="white") +             
            annotate("text",x = as.Date("2020-01-13", format="%Y-%m-%d"), y = 125, label = paste0("Quorum: ", QUORUM)) + 
            annotate("text",x = as.Date("2020-02-07", format="%Y-%m-%d"), y = 60, label = "Annual Meeting", angle = 90) + 
            theme_light() + 
            theme(legend.position = "none")

ggsave("graphs/vote-tracking-2020.png")
ggsave("graphs/vote-tracking-2020.pdf")
