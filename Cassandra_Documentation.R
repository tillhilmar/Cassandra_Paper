library("tidyverse")
library("lubridate")
library("ggthemes")
library("magrittr")
library("purrr")
library("tibble")
library("tidyverse")
library("dplyr")
library("tidyr")
library("readxl")
library("writexl")
library("stopwords")
library("quanteda")
library("quanteda.textstats")
library("quanteda.textmodels")
library("kableExtra")
library("ggplot2")

# AFD full dataset (no retweets, and up to the end of March 2023: ALL_AFD_FINAL)
# FPOE: ALL_FPOE
# creating a new FPOE df that only runs until end of March to align with AfD data
ALL_FPOE_noRT_new <- ALL_FPOE_noRT[ALL_FPOE_noRT$created_at <= "2023-03-31T23:59:59.000Z", ]

# overview of users in this data 
# differentiate the "tweet_count" between data from 2023 and from 2025
# I am only including AFD tweets after Feburary 06, 2013, which is the date when the party was founded

# Convert to POSIXct
ALL_AFD_FINAL$created_at_parsed <- as.POSIXct(
  ALL_AFD_FINAL$created_at,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)
# Filter to keep only tweets from Feb 6, 2013 onward
ALL_AFD_FINAL <- ALL_AFD_FINAL[ALL_AFD_FINAL$created_at_parsed >= as.POSIXct("2013-02-06T00:00:00.000Z", tz = "UTC"), ]

# Create a new "tweet_count" variable here that captures only the number of tweets in this period
ALL_AFD_FINAL <- ALL_AFD_FINAL %>%
  group_by(author_id) %>%
  mutate(tweet_count1 = n_distinct(unique_tweet_id)) %>%
  ungroup()

## creating the overview table
ALL_AFD_users_table <- subset(as.data.frame(ALL_AFD_FINAL), 
                              select = c("name", "username", "user_description", "location", "user_created_at", "tweet_count1", "downloaded", "author_id"))
ALL_AFD_users_table_unique <- ALL_AFD_users_table %>%
  distinct(author_id, .keep_all = TRUE)
ALL_AFD_users_table_unique$user_created_at <- as.Date(ALL_AFD_users_table_unique$user_created_at)

ALL_AFD_users_table_unique <- ALL_AFD_users_table_unique %>%
  arrange(as.integer(downloaded), user_created_at)

ALL_AFD_users_table_unique[1:230, 1:7] %>%
  kbl(caption = "Overview of AfD Accounts in this Sample") %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>%
  save_kable(file = "Overview_AfD_Users_new.html", self_contained = T)

ALL_FPOE_noRT_new <- ALL_FPOE_noRT_new %>%
  group_by(author_id) %>%
  mutate(tweet_count1 = n_distinct(unique_tweet_id)) %>%
  ungroup()

ALL_FPOE_users_table <- subset(ALL_FPOE_noRT_new[c("name", "username", "user_description", "location", "user_created_at", "tweet_count1")])
ALL_FPOE_users_table_unique <- ALL_FPOE_users_table %>%
  distinct(name, .keep_all = TRUE)
ALL_FPOE_users_table_unique$user_created_at <- as.Date(ALL_FPOE_users_table_unique$user_created_at)
ALL_FPOE_users_table_unique <- ALL_FPOE_users_table_unique %>% arrange(user_created_at)

ALL_FPOE_users_table_unique[1:38, 1:6] %>%
  kbl(caption = "Overview of FPÖ Accounts in this Sample") %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>%
  save_kable(file = "Overview_FPOE_Users_new.html", self_contained = T)

# basic plot
library(ggthemes)
wsjPal <- c('#1C366B','#C4CFD0','#1DACE8','#F24D29','#76A08A','#9A872D')

ALL_AFD_plot <- ALL_AFD_FINAL %>% select("unique_tweet_id", "created_at")
ALL_AFD_plot$created_at <- as.Date(ALL_AFD_plot$created_at)

# Create a histogram of tweet counts over time
ggplot(ALL_AFD_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "n = 434229", subtitle = "AfD Tweeting") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

ALL_FPOE_plot <- ALL_FPOE_noRT_new %>% select("unique_tweet_id", "created_at")
ALL_FPOE_plot$created_at <- as.Date(ALL_FPOE_plot$created_at)

# Create a histogram of tweet counts over time
ggplot(ALL_FPOE_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "n = 81455", subtitle = "FPÖ Tweeting") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

# keywords based on the revision June 2025

climate_impact <- c(
  "klima", "Treibhaus", "Erwärmung", "CO2", "Emission", "Kohlenstoff", "Ressourcenverbrauch",
  "Biodiversität", "Natur", "Umwelt", "Ökol.*", "Fußabdruck",
  "Waldbrände", "Artensterben", "Luftverschmutzung", "Ozeanversauerung", "Weltmeere", 
  "Hitzewelle", "Dürre", "Überschwemmung", "Hochwasser", "Naturkatastrophe"
)

energy_terms <- c(
  "Energie", "\\bWind\\w*",
  "Solarenergie", "Photovoltaik", "Sonne",
  "Geothermie", "Biomasse",
  "Erneuerbar", "Erneuerbare", "Energiewende",
  "Wasserstoff", "Speicherkapazität", "Netzausbau",
  "Kernkraft", "AKW", "Atom.*",
  "Braunkohle", "Fossil", "Kohleausstieg", "Dekarbonisierung"
)

activism_terms <- c(
  "FFF", "Fridays for Future", "Greta", "Thunberg",
  "Letzte Generation", "Klimastreik", "Klimaklage",
  "Klimagerechtigkeit", "Klimabewegung", "Klimaprotest",
  "Scientist Rebellion", "Ende Gelände",
  "Extinction Rebellion",
  "Klimablockade", "Kohleblockade",
  "Lützerath",
  "Aktivist.*"
)

policy_terms <- c(
  "Klimapolitik", "Klimaziel", "Klimaneutral", "Klimaneutralität", 
  "Pariser Abkommen", "1,5-Grad", "Emissionshandel", "Netto-Null", "Klimaschutzgesetz", "CO2-Bepreisung"
)
# any word that contains the element "Klima" - like "Klimabonus" - will also appear in that list

climate_keywords <- c(climate_impact, energy_terms, activism_terms, policy_terms)

subset_ALL_AFD_climate_crisis <- ALL_AFD_FINAL[grepl(paste(climate_keywords, collapse = "|"), ALL_AFD_FINAL$text, ignore.case = TRUE), ]
subset_ALL_FPOE_climate_crisis <- ALL_FPOE_noRT_new[grepl(paste(climate_keywords, collapse = "|"), ALL_FPOE_noRT_new$text, ignore.case = TRUE), ]

# check what has been extracted:
ALL_AFD_FINAL %>%
  filter(str_detect(text, regex(paste(climate_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  mutate(matched = str_extract(text, regex(paste(climate_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  select(text, matched) %>%
  View()

#cleaning some noise (duplicate tweets that are not flagged as such in the data) from the FPOE df
ids_to_remove <- c(
  373877904587108352,
  373877904863920128,
  373877905266573313,
  373878023915073537,
  373878150717247488,
  373878155553300480,
  373878153946869760,
  373878153858392064,
  373878150717247488,
  373878089400328192,
  373878086225649664,
  373878085810413568,
  373878085722320896,
  373878085386788864,
  373878023915073537,
  373878023743086592,
  373878021893419008,
  373878020513484800,
  373877967375859713,
  373877966620884992,
  373877966549549056,
  373877965433892864,
  373877963831668736,
  373877905488896000,
  373877905266573313,
  373877904863920128,
  373877904717123584,
  373877904587108352)

subset_ALL_FPOE_climate_crisis <- subset_ALL_FPOE_climate_crisis[!subset_ALL_FPOE_climate_crisis$unique_tweet_id %in% ids_to_remove, ]

# plotting climate discourse -----
library(ggthemes)
wsjPal <- c('#1C366B','#C4CFD0','#1DACE8','#F24D29','#76A08A','#9A872D')

subset_ALL_AFD_climate_crisis_plot <- subset_ALL_AFD_climate_crisis %>% select("unique_tweet_id", "created_at")
subset_ALL_AFD_climate_crisis_plot$created_at <- as.Date(subset_ALL_AFD_climate_crisis_plot$created_at)

# Create a histogram of tweet counts over time
ggplot(subset_ALL_AFD_climate_crisis_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "n = 21511", subtitle = "AfD Tweeting on climate") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))


subset_ALL_FPOE_climate_crisis_plot <- subset_ALL_FPOE_climate_crisis %>% select("unique_tweet_id", "created_at")
subset_ALL_FPOE_climate_crisis_plot$created_at <- as.Date(subset_ALL_FPOE_climate_crisis_plot$created_at)

# Create a histogram of tweet counts over time

ggplot(subset_ALL_FPOE_climate_crisis_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "n = 1441", subtitle = "FPÖ Tweeting on climate") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

# Summarize tweet counts by date
library(dplyr)

tweet_summary <- subset_ALL_FPOE_climate_crisis_plot %>%
  dplyr::count(created_at)

# Plot the smoothed line
ggplot(tweet_summary, aes(x = created_at, y = n)) + 
  geom_line(color = "#3A5489") + 
  geom_smooth(method = "loess", formula = 'y ~ x', color = "#FF5733", se = FALSE) +  
  labs(x = "Date", y = "Tweet Count", title = "n = 1441", subtitle = "FPÖ Tweeting on climate") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))


# plotting them together, showing just climtae talk ----
subset_ALL_FPOE_climate_crisis_plot$party <- "FPÖ"
subset_ALL_AFD_climate_crisis_plot$party <- "AfD"

plot_together <- rbind(subset_ALL_FPOE_climate_crisis_plot, subset_ALL_AFD_climate_crisis_plot)

library(scales)  # For scale_x_date and date formatting

library(ggplot2)
library(dplyr)

# Preprocess the data: Summarize tweet counts by date and party
plot_data <- plot_together %>%
  group_by(created_at, party) %>%
  dplyr::summarise(tweet_count = n(), .groups = 'drop')

ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = party, color = party)) +
  geom_line() +
  scale_color_manual(values = c("AfD" = "#1F4E79", "FPÖ" = "#1EB5D6")) +
  labs(x = "Date", y = "Tweet Count", title = "Tweeting on Climate by Party Over Time", subtitle = "Line Plot Differentiated by Party") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +  
  theme_wsj() + 
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# plot with Y axis labeled for resubmission

plot_data <- plot_together %>%
  group_by(created_at, party) %>%
  dplyr::summarise(tweet_count = n(), .groups = 'drop')

ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = party, color = party)) +
  geom_line() +
  scale_color_manual(values = c("AfD" = "#1F4E79", "FPÖ" = "#1EB5D6")) +
  labs(x = "Date", 
       y = "Number of Tweets per Day",
       title = "Tweeting on Climate by Party Over Time", 
       subtitle = "Total: 22,952 Tweets (21,511 by AfD politicans; 1,441 by FPÖ politicians)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") + 
  theme_wsj() + 
  theme(
    plot.title = element_text(size = 11, family = "sans"),
    plot.subtitle = element_text(size = 10, family = "sans"),
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 10, family = "sans"),
    axis.title.y = element_text(size = 10, family = "sans") 
  )

# plot with higher resolution

# Create the plot and assign it to an object
climate_plot <- ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = party, color = party)) +
  geom_line() +
  scale_color_manual(values = c("AfD" = "#1F4E79", "FPÖ" = "#1EB5D6")) +
  labs(x = "Date", 
       y = "Number of Tweets per Day",
       title = "Tweeting on Climate by Party Over Time", 
       subtitle = "Total: 22,952 Tweets (21,511 by AfD politicans; 1,441 by FPÖ politicians)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(
    plot.title = element_text(size = 11, family = "sans"),
    plot.subtitle = element_text(size = 10, family = "sans"),
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 10, family = "sans"),
    axis.title.y = element_text(size = 10, family = "sans")
  )

# Save the plot with double resolution (e.g., 600 dpi)
ggsave("climate_tweet_plot_highres.png", plot = climate_plot, 
       width = 10, height = 6, dpi = 600, bg = "white")

# plot contrasting climate communication with rest  ----

# create a new variable in the main df that contains the "climate" or "other" options
ALL_AFD_FINAL$climatevariable <- "other"
ALL_AFD_FINAL$climatevariable[ALL_AFD_FINAL$unique_tweet_id %in% subset_ALL_AFD_climate_crisis$unique_tweet_id] <- "climate"
ALL_AFD_FINAL_climatevariable <- ALL_AFD_FINAL

length(ALL_AFD_FINAL_climatevariable$climatevariable)

plot_ALL_AFD_FINAL_climatevariable <- ALL_AFD_FINAL_climatevariable %>% select("unique_tweet_id", "created_at", "climatevariable")
plot_ALL_AFD_FINAL_climatevariable$created_at <- as.Date(plot_ALL_AFD_FINAL_climatevariable$created_at)

plot_data <- plot_ALL_AFD_FINAL_climatevariable %>%
  group_by(created_at, climatevariable) %>%
  dplyr::summarise(tweet_count = n(), .groups = 'drop')

climate_plot_AFD <-ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = climatevariable, color = climatevariable)) +
  geom_line() +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", 
       y = "Number of Tweets per Day",
       title = "AfD Tweeting on Climate and Other Topics Over Time", subtitle = "Climate: n=21,511; Other: n=412,718. Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, family = "sans"),  
        axis.title.y = element_text(size = 10, family = "sans"))

ggsave("climate_tweet_plot_AFD_highres.png", plot = climate_plot_AFD, 
       width = 10, height = 6, dpi = 600, bg = "white")

# plots for max data 
max_tweet_data <- plot_data %>%
  group_by(created_at, climatevariable) %>%
  dplyr::summarise(max_tweet_count = max(tweet_count)) %>%
  dplyr::ungroup()

ggplot(max_tweet_data, aes(x = as.Date(created_at), y = max_tweet_count, group = climatevariable, color = climatevariable)) +
  geom_line() +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", y = "Tweet Count", title = "AfD Tweeting on Climate and Other Topics Over Time", subtitle = "Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(max_tweet_data, aes(x = as.Date(created_at), y = max_tweet_count, group = climatevariable, color = climatevariable)) +
  geom_smooth(method = "gam", method.args = list(family = "poisson"), se = FALSE) +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", y = "Tweet Count", title = "AfD Tweeting on Climate and Other Topics Over Time", subtitle = "Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = 0)

# same operation for FPOE
ALL_FPOE_noRT_new$climatevariable <- "other"
ALL_FPOE_noRT_new$climatevariable[ALL_FPOE_noRT_new$unique_tweet_id %in% subset_ALL_FPOE_climate_crisis$unique_tweet_id] <- "climate"
ALL_FPOE_noRT_new_climatevariable <- ALL_FPOE_noRT_new

plot_ALL_FPOE_noRT_climatevariable <- ALL_FPOE_noRT_climatevariable %>% select("unique_tweet_id", "created_at", "climatevariable")
plot_ALL_FPOE_noRT_climatevariable$created_at <- as.Date(plot_ALL_FPOE_noRT_climatevariable$created_at)

plot_data <- plot_ALL_FPOE_noRT_climatevariable %>%
  group_by(created_at, climatevariable) %>%
  dplyr::summarise(tweet_count = n(), .groups = 'drop')

climate_plot_FPOE <- ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = climatevariable, color = climatevariable)) +
  geom_line() +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", 
       y = "Number of Tweets per Day",
       title = "FPÖ Tweeting on Climate and Other Topics Over Time", subtitle = "Climate: n=1,441, Other: n=80,014. Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, family = "sans"),  
        axis.title.y = element_text(size = 10, family = "sans"))

ggsave("climate_tweet_plot_FPOE_highres.png", plot = climate_plot_FPOE, 
       width = 10, height = 6, dpi = 600, bg = "white")

# with smoothened line 
ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = climatevariable, color = climatevariable)) +
  geom_smooth(method = "gam", method.args = list(family = "poisson"), se = FALSE) +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", y = "Tweet Count", title = "FPOE Tweeting on Climate and Other Topics Over Time", subtitle = "Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = 0)

# Another plot for FPÖ that begins in 2014 to facilitate comparison

ALL_FPOE_noRT_climatevariable_shortened <- ALL_FPOE_noRT_climatevariable[as.Date(ALL_FPOE_noRT_climatevariable$created_at) > as.Date("2014-01-01"), ]

plot_ALL_FPOE_noRT_climatevariable_shortened <- ALL_FPOE_noRT_climatevariable_shortened %>% select("unique_tweet_id", "created_at", "climatevariable")
plot_ALL_FPOE_noRT_climatevariable_shortened$created_at <- as.Date(plot_ALL_FPOE_noRT_climatevariable_shortened$created_at)

plot_data <- plot_ALL_FPOE_noRT_climatevariable_shortened %>%
  group_by(created_at, climatevariable) %>%
  dplyr::summarise(tweet_count = n(), .groups = 'drop')

ggplot(plot_data, aes(x = as.Date(created_at), y = tweet_count, group = climatevariable, color = climatevariable)) +
  geom_line() +
  scale_color_manual(values = c("climate" = "#006400", "other" = "#A9A9A9")) +
  labs(x = "Date", y = "Tweet Count", title = "FPOE Tweeting on Climate and Other Topics Over Time", subtitle = "Retweets Excluded") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# comparing the two datasets using chi-square comparison

subset_ALL_FPOE_climate_crisis$party <- "FPÖ"
subset_ALL_AFD_climate_crisis$party <- "AfD"

combined_df <- bind_rows(subset_ALL_FPOE_climate_crisis, subset_ALL_AFD_climate_crisis)

corp <- corpus(combined_df)
toks <- tokens(corp)
dfm_entities <- dfm(toks, remove_punct = TRUE)
dfm_entities_clean <- dfm_remove(dfm_entities, c(ger_stopwords, custom_stopwords))
dfm_entities_clean <- dfm_remove(dfm_entities_clean, "\\p{N}+", valuetype = "regex")
dfm_entities_clean <- dfm_remove(dfm_entities_clean, pattern = "[+➡🇩🇪⬇|]", valuetype = "regex")
dfm_entities_clean <- dfm_remove(dfm_entities_clean, pattern = "amp", valuetype = "regex")

pop <- ifelse(docvars(dfm_entities_clean, "party") == 'AfD', "AfD", "FPÖ")
dfmat1 <- dfm(dfm_entities_clean, groups = pop, tolower = FALSE, verbose = FALSE)
tstat1 <- textstat_keyness(dfmat1, target = "AfD")
quanteda.textplots::textplot_keyness(tstat1, color = c("darkblue", "lightblue"), n = 18)

p <- quanteda.textplots::textplot_keyness(tstat1, color = c("darkblue", "lightblue"), n = 18)
ggsave("keyness_plot_highres.png", plot = p, width = 10, height = 7, dpi = 300)

# checking the popularity of climate-tweets relative to other tweets

# Filter for rows where climatevariable is "climate"
climate_tweets_AfD <- ALL_AFD_FINAL_climatevariable %>%
  filter(climatevariable == "climate")
# Calculate the average retweet count for each author_id
author_retweet_averages <- climate_tweets_AfD %>%
  group_by(author_id) %>%
  dplyr::summarise(average_retweet_count = mean(retweet_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
climate_tweets_AfD <- climate_tweets_AfD %>%
  left_join(author_retweet_averages, by = "author_id")
climate_tweets_unique_authors_AfD <- climate_tweets_AfD %>%
  distinct(author_id, .keep_all = TRUE)

# Filter for rows where climatevariable is not "climate"
other_tweets_AfD <- ALL_AFD_FINAL_climatevariable %>%
  filter(climatevariable == "other")
# Calculate the average retweet count for each author_id
author_retweet_averages <- other_tweets_AfD %>%
  group_by(author_id) %>%
  dplyr::summarise(average_retweet_count = mean(retweet_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
other_tweets_AfD <- other_tweets_AfD %>%
  left_join(author_retweet_averages, by = "author_id")
other_tweets_unique_authors_AfD <- other_tweets_AfD %>%
  distinct(author_id, .keep_all = TRUE)

sum(climate_tweets_unique_authors_AfD$average_retweet_count)
sum(other_tweets_unique_authors_AfD$average_retweet_count)

# show mean and median scores
mean(climate_tweets_unique_authors_AfD$average_retweet_count)
mean(other_tweets_unique_authors_AfD$average_retweet_count)

median(climate_tweets_unique_authors_AfD$average_retweet_count)
median(other_tweets_unique_authors_AfD$average_retweet_count)

# Same operation for likes:
# Calculate the average retweet count for each author_id
author_likes_averages <- climate_tweets_AfD %>%
  group_by(author_id) %>%
  dplyr::summarise(average_like_count = mean(like_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
climate_tweets_AfD_likes <- climate_tweets_AfD %>%
  left_join(author_likes_averages, by = "author_id")
climate_tweets_unique_authors_AfD_likes <- climate_tweets_AfD_likes %>%
  distinct(author_id, .keep_all = TRUE)

# Calculate the average retweet count for each author_id
author_likes_averages <- other_tweets_AfD %>%
  group_by(author_id) %>%
  dplyr::summarise(average_like_count = mean(like_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
other_tweets_AfD_likes <- other_tweets_AfD %>%
  left_join(author_likes_averages, by = "author_id")
other_tweets_unique_authors_AfD_likes <- other_tweets_AfD_likes %>%
  distinct(author_id, .keep_all = TRUE)

sum(climate_tweets_unique_authors_AfD_likes$average_like_count)
sum(other_tweets_unique_authors_AfD_likes$average_like_count)

mean(climate_tweets_unique_authors_AfD_likes$average_like_count)
mean(other_tweets_unique_authors_AfD_likes$average_like_count)

median(climate_tweets_unique_authors_AfD_likes$average_like_count)
median(other_tweets_unique_authors_AfD_likes$average_like_count)

# Same operation for FPOE:
# Filter for rows where climatevariable is "climate"
climate_tweets_FPOE <- ALL_FPOE_noRT_new_climatevariable %>%
  filter(climatevariable == "climate")
# Calculate the average retweet count for each author_id
author_retweet_averages <- climate_tweets_FPOE %>%
  group_by(author_id) %>%
  dplyr::summarise(average_retweet_count = mean(retweet_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
climate_tweets_FPOE <- climate_tweets_FPOE %>%
  left_join(author_retweet_averages, by = "author_id")
climate_tweets_unique_authors_FPOE <- climate_tweets_FPOE %>%
  distinct(author_id, .keep_all = TRUE)

# Filter for rows where climatevariable is not "climate"
other_tweets_FPOE <- ALL_FPOE_noRT_climatevariable %>%
  filter(climatevariable == "other")
# Calculate the average retweet count for each author_id
author_retweet_averages <- other_tweets_FPOE %>%
  group_by(author_id) %>%
  dplyr::summarise(average_retweet_count = mean(retweet_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
other_tweets_FPOE <- other_tweets_FPOE %>%
  left_join(author_retweet_averages, by = "author_id")
other_tweets_unique_authors_FPOE <- other_tweets_FPOE %>%
  distinct(author_id, .keep_all = TRUE)

sum(climate_tweets_unique_authors_FPOE$average_retweet_count)
sum(other_tweets_unique_authors_FPOE$average_retweet_count)

mean(climate_tweets_unique_authors_FPOE$average_retweet_count)
mean(other_tweets_unique_authors_FPOE$average_retweet_count)

median(climate_tweets_unique_authors_FPOE$average_retweet_count)
median(other_tweets_unique_authors_FPOE$average_retweet_count)

# Same for likes
# Calculate the average retweet count for each author_id
author_likes_averages <- climate_tweets_FPOE %>%
  group_by(author_id) %>%
  dplyr::summarise(average_like_count = mean(like_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
climate_tweets_FPOE_likes <- climate_tweets_FPOE %>%
  left_join(author_likes_averages, by = "author_id")
climate_tweets_unique_authors_FPOE_likes <- climate_tweets_FPOE_likes %>%
  distinct(author_id, .keep_all = TRUE)

# Calculate the average retweet count for each author_id
author_likes_averages <- other_tweets_FPOE %>%
  group_by(author_id) %>%
  dplyr::summarise(average_like_count = mean(like_count, na.rm = TRUE), .groups = 'drop')
# Join the average retweet count back to the climate_tweets
other_tweets_FPOE_likes <- other_tweets_FPOE %>%
  left_join(author_likes_averages, by = "author_id")
other_tweets_unique_authors_FPOE_likes <- other_tweets_FPOE_likes %>%
  distinct(author_id, .keep_all = TRUE)

sum(climate_tweets_unique_authors_FPOE_likes$average_like_count)
sum(other_tweets_unique_authors_FPOE_likes$average_like_count)

mean(climate_tweets_unique_authors_FPOE_likes$average_like_count)
mean(other_tweets_unique_authors_FPOE_likes$average_like_count)

median(climate_tweets_unique_authors_FPOE_likes$average_like_count)
median(other_tweets_unique_authors_FPOE_likes$average_like_count)

# creating the sample for the qualitative analysis

# Making sure that segments that were coded in an earlier round are not included again:
older_coding_list <- intial_perid$unique_tweet_id

# first step - initial period - the first couple of years, until 2017 ---
initial_period_new <- subset(combined_df, created_at < "2017-01-01T00:00:00.000Z")
initial_period_new <- intial_period_new[!(intial_period_new$unique_tweet_id %in% older_coding_list), ]
initial_period_output_new <- subset(initial_period_new[c("created_at", "text", "username", "user_description", "author_id", "party")] )
initial_period_output_new <- initial_period_output_new %>%
  arrange(created_at)
table(initial_period_output_new$party)
write_xlsx(initial_period_output_new, "initial_period_output_new_without_older.xlsx")
# note: merged the file manually, old coding with this new segment, it is called _FINAL

table(initial_period_new$party)
table(intial_period_new$party)

# top dates ---
# AfD: Counting frequencies of dates and sorting to find top dates
top_AfD_dates <- subset_ALL_AFD_climate_crisis_plot %>%
  dplyr::group_by(created_at) %>%
  dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
  dplyr::arrange(dplyr::desc(count))
# sub-sampling by top dates
top_AfD_dates_select <- top_AfD_dates[1:10, ]
subset_ALL_AFD_climate_crisis_sample <- subset_ALL_AFD_climate_crisis_noRT
subset_ALL_AFD_climate_crisis_sample$created_at <- as.Date(subset_ALL_AFD_climate_crisis_sample$created_at)
subset_ALL_AFD_climate_crisis_sample <- subset_ALL_AFD_climate_crisis_sample[subset_ALL_AFD_climate_crisis_sample$created_at %in% top_AfD_dates_select$created_at, ]

subset_top10dates_AFD <- subset_ALL_AFD_climate_crisis_sample[c("created_at", "text", "username", "user_description", "author_id", "party")]
subset_top10dates_AFD <- subset_top10dates_AFD %>%
  arrange(created_at)
write_xlsx(subset_top10dates_AFD, "subset_top10dates_AFD_new.xlsx")

# FPO: Counting frequencies of dates and sorting to find top dates
top_FPOE_dates <- subset_ALL_FPOE_climate_crisis_plot %>%
  dplyr::group_by(created_at) %>%
  dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
  dplyr::arrange(dplyr::desc(count))
# sub-sampling by top dates
top_FPOE_dates_select <- top_FPOE_dates[1:10, ]
subset_ALL_FPOE_climate_crisis_sample <- subset_ALL_FPOE_climate_crisis
subset_ALL_FPOE_climate_crisis_sample$created_at <- as.Date(subset_ALL_FPOE_climate_crisis_sample$created_at)
subset_ALL_FPOE_climate_crisis_sample <- subset_ALL_FPOE_climate_crisis_sample[subset_ALL_FPOE_climate_crisis_sample$created_at %in% top_FPOE_dates_select$created_at, ]

subset_top10dates_FPOE <- subset_ALL_FPOE_climate_crisis_sample[c("created_at", "text", "username", "user_description", "author_id", "party")]
subset_top10dates_FPOE <- subset_top10dates_FPOE %>%
  arrange(created_at)
write_xlsx(subset_top10dates_FPOE, "subset_top10dates_FPOE_new.xlsx")

# popularity sample

older_coding_list_retweets_AFD <- top_100_retweets_AFD$unique_tweet_id

top_100_retweets_AFD_new <- subset_ALL_AFD_climate_crisis %>%
  arrange(desc(retweet_count)) %>%
  slice(1:100)
top_100_retweets_AFD_new <- top_100_retweets_AFD_new[!(top_100_retweets_AFD_new$unique_tweet_id %in% older_coding_list_retweets_AFD), ]

short_top100retweets_AFD_new <- top_100_retweets_AFD_new[c("unique_tweet_id", "created_at", "text", "retweet_count", "username", "user_description", "author_id", "party")]
short_top100retweets_AFD_new <- short_top100retweets_AFD_new %>%
  arrange(desc(retweet_count))
write_xlsx(short_top100retweets_AFD_new, "short_top100retweets_AFD_new.xlsx")

# extracting top liked tweets from AfD
top_100_liked_AFD_new <- subset_ALL_AFD_climate_crisis %>%
  arrange(desc(like_count)) %>%
  slice(1:100)

short_top100likes_AFD_new <- top_100_liked_AFD_new[c("unique_tweet_id","created_at", "text", "like_count", "username", "user_description", "author_id", "party")]
short_top100likes_AFD_new <- short_top100likes_AFD_new %>%
  arrange(desc(like_count))
write_xlsx(short_top100likes_AFD_new, "short_top100likes_AFD_new.xlsx")

short_top100_substracted_AFD <- anti_join(short_top100likes_AFD, short_top100retweets_AFD, by = "unique_tweet_id")

# extracting top retweeted tweets from FPOE
top_100_retweets_FPOE_new <- subset_ALL_FPOE_climate_crisis %>%
  arrange(desc(retweet_count)) %>%
  slice(1:100)
# extracting top liked tweets from FPOE
top_100_liked_FPOE_new <- subset_ALL_FPOE_climate_crisis %>%
  arrange(desc(like_count)) %>%
  slice(1:100)

short_top100retweets_FPOE_new <- top_100_retweets_FPOE_new[c("unique_tweet_id", "created_at", "text", "retweet_count", "username", "user_description", "author_id", "party")]
short_top100retweets_FPOE_new <- short_top100retweets_FPOE_new %>%
  arrange(desc(retweet_count))
write_xlsx(short_top100retweets_FPOE_new, "short_top100retweets_FPOE_new.xlsx")

short_top100likes_FPOE_new <- top_100_liked_FPOE_new[c("unique_tweet_id", "created_at", "text", "like_count", "username", "user_description", "author_id", "party")]
short_top100likes_FPOE_new <- short_top100likes_FPOE_new %>%
  arrange(desc(like_count))
write_xlsx(short_top100likes_FPOE_new, "short_top100likes_FPOE_new.xlsx")

short_top100_substracted_FPOE <- anti_join(short_top100likes_FPOE, short_top100retweets_FPOE, by = "unique_tweet_id")

# checking the dimension of the corpus
table(intial_perid$party)
table(initial_period_new$party)

final_qualitative_corpus_new <- bind_rows(
  select(intial_perid, -created_at),
  select(initial_period_new, -created_at),
  select(subset_ALL_FPOE_climate_crisis_sample, -created_at),
  select(subset_ALL_AFD_climate_crisis_sample, -created_at),
  select(top_100_liked_AFD_new, -created_at),
  select(top_100_liked_FPOE_new, -created_at),
  select(top_100_retweets_AFD_new, -created_at),
  select(top_100_retweets_FPOE_new, -created_at)
)

final_qualitative_corpus_new_unique <- final_qualitative_corpus_new %>%
  distinct(unique_tweet_id, .keep_all = TRUE)

# extracting top liked tweets
top_20_liked_AFD <- subset_ALL_AFD_climate_crisis_sample %>%
  arrange(desc(like_count)) %>%
  slice(1:20)
short_top20likes_AFD <- top_20_liked_AFD[c("unique_tweet_id", "created_at", "text", "like_count", "username", "user_description", "author_id", "party")]
short_top20likes_AFD <- short_top20likes_AFD %>%
  arrange(desc(like_count))
write_xlsx(short_top20likes_AFD, "short_top20likes_AFD.xlsx")

top_20_liked_FPOE <- subset_ALL_FPOE_climate_crisis_sample %>%
  arrange(desc(like_count)) %>%
  slice(1:20)
short_top20likes_FPOE <- top_20_liked_FPOE[c("unique_tweet_id", "created_at", "text", "like_count", "username", "user_description", "author_id", "party")]
short_top20likes_FPOE <- short_top20likes_FPOE %>%
  arrange(desc(like_count))
write_xlsx(short_top20likes_FPOE, "short_top20likes_FPOE.xlsx")
