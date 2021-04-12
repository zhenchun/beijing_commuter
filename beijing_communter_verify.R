require(tidyverse)
require(chron)

setwd("F:/Box Sync/micro-environment/beijing_commuter/beijing_commuter (zh133@duke.edu)/data_verify")
data <- read_csv("beijing.csv")

# verify weekdays
data <- data %>%
            separate(start_time, c("start_date", "start_time"), sep = " ") %>%
                separate(end_time, c("end_date", "end_time"), sep = " ") %>%
                    separate(start_date, c("month", "day", "year"), sep = "/") %>%
                        unite(col = date_ver, year, month, day, sep = "/") %>%
                            mutate(date_ver = as.Date(date_ver)) %>%
                                mutate(weekdays_ver = ifelse(is.weekend(date_ver) == FALSE, "weekday", "weekend")) # no differences detected

# verify pattern
# peak is defined as 7:00 - 9:00 (420 minutes - 540 minutes) as well as 17:00 - 20:00 (1020 minutes - 1200 minutes)
data <- data %>%
            separate(start_time, c("start_hour", "start_minute"), sep = ":") %>%
                separate(end_time, c("end_hour", "end_minute"), sep = ":") %>%
                    mutate(start_hour = as.numeric(start_hour),
                           start_minute = as.numeric(start_minute),
                           end_hour = as.numeric(end_hour),
                           end_minute = as.numeric(end_minute)) %>%
                        mutate(start_min = start_hour * 60 + start_minute) %>%
                            mutate(end_min = end_hour * 60 + end_minute) %>%
                                mutate(mid_min = (end_min + start_min) / 2) %>%
                                    mutate(pattern_ver = case_when(mid_min < 420 ~ "off.peak",
                                                                   mid_min >= 420 & mid_min <= 540 ~ "peak",
                                                                   mid_min > 540 & mid_min < 1020 ~ "off.peak",
                                                                   mid_min >= 1020 & mid_min <= 1200 ~ "peak",
                                                                   mid_min > 1200 ~ "off.peak")) %>%
                                        mutate(pattern_ver = ifelse(weekdays == "weekend", "off.peak", pattern_ver)) # no differences detected
