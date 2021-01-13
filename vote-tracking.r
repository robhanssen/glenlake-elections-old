library(tidyverse)
library(lubridate)

votes <- read_csv("vote-tracking.csv") %>% mutate(date = as.Date(date, format="%Y-%m-%d"))

# caption generator
lastgen = format(today(), format="%b %d, %Y")
lastupdate = format(max(votes$date), format="%b %d, %Y")
capt = paste0("\U00A9 2021, Glenlake Homeowners Association\nLast updated ", lastgen, "\nLast data entry: ", lastupdate)

# y-axis max
ymax = ifelse(max(votes$votesreceived)<120, 160, ((max(votes$votesreceived)*1.33)%/%10 + 1)*10 )


votes %>% ggplot + aes(x=date, y=votesreceived) + 
            geom_point() + 
            geom_smooth(method="lm", lty=2, color="gray") + 
            scale_x_date(date_breaks="1 week", date_labels = "%b %d", limit=c(as.Date("2021-01-11"),as.Date("2021-02-10"))) + 
            scale_y_continuous(limit=c(0,ymax), breaks=seq(0,500,30)) + 
            labs(x="Date", y="Votes received", caption=capt)  +
            ggtitle("Votes received for the 2021 Glen Lake Board of Directors election") +
            theme_light() + 
            geom_hline(yintercept=120, lty=2) + 
            geom_vline(xintercept=as.Date("2021-02-06"), lty=2, color="red") + 
            annotate("text",x=as.Date("2021-01-12", format="%Y-%m-%d"),y=125,label="Quorum") + 
            annotate("text",x=as.Date("2021-02-05", format="%Y-%m-%d"),y=60,label="Annual Meeting date", angle=90)

ggsave("graphs/vote-tracking.png")
