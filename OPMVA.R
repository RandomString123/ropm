# OPMVA - Charts and Graphs for the Department of Veterans Affairs
#    Copyright (C) 2017  Matthew Churilla (matthew.churilla@outlook.com)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(modelr)
library(reshape2)
### This file performs the same functions as our main charting file but only for a specific organization
### SECURITY AND EXCHANGE COMISSION.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Calculate summary statistics for our charts
#local_data <- model %>% ungroup() %>% filter(AGENCY == "SECURITIES AND EXCHANGE COMMISSION")
local_data <- model %>% ungroup() %>% filter(AGENCY == "VETERANS ADMINISTRATION")
#local_data <- model %>% ungroup() %>% filter(AGENCY == "OFFICE OF PERSONNEL MANAGEMENT")
#local_data <- model %>% ungroup() %>% filter(AGENCY == "APPALACHIAN REGIONAL COMMISSION")

#Ensure result is sorted by PSEUDO then GROUP ID
# How long each employment segment is and length between employment segments for those with more than one.
los_summary <- local_data %>% group_by(`PSEUDO-ID`, `group-id`) %>% summarize(los = n(), start = as.integer(first(year)), end = as.integer(last(year)), los_level = last(`LOS LEVEL`)) %>% group_by(`PSEUDO-ID`) %>% mutate(length_between = start - lag(end) - 1)
# Number of service segments with shortest and longest recorded.
num_los_summary <- los_summary %>% group_by(`PSEUDO-ID`) %>% summarize(num = n(), shortest = min(los), longest = max(los))
# Total employed in any year
tot_by_year <- local_data %>% group_by(year) %>% summarize(count = n())
# Arrivals, first year of service.
arr_by_year <- los_summary %>% group_by(start) %>% summarize(starts = n())
arr_by_year[arr_by_year$start == 1973, 2] <- NA
# Departures, Year after last year of service
dep_by_year <- los_summary %>% group_by(end) %>% summarize(ends = n())
dep_by_year[, 2] <- lag(dep_by_year$ends)
# Table of count, arrivals, departures
turnover <- merge(merge(tot_by_year, arr_by_year, by.x = "year", by.y = "start"), dep_by_year, by.x = "year", by.y = "end")
turnover <- as.tibble(turnover) %>% mutate(year = as.integer(year), pct = ends / ((count + lag(count)) / 2))

# Length of Service histogram area chart
ggplot(data = local_data, aes(x = year, fill = `LOS LEVEL`)) +
geom_area(stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "VA Length of Service Over Time") +
#scale_y_continuous(labels = scales::percent) +
scale_y_continuous(labels = scales::comma) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_summary_hist_va.png", height = 5, width = 10)

#Summary to compare to whole Non-DoD
summary(los_summary$los, digits = 2)

#Histogram of YOS by departing employees
ggplot(data = (los_summary %>% filter(end != 2013)), aes(x = end, fill = los_level)) +
geom_area(stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "VA Length of Service for Depatring Employees") +
scale_y_continuous(labels = scales::comma) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_depart_summary_hist_va.png", height = 5, width = 10)


pct_summary <- local_data %>% ungroup() %>% group_by(year) %>% summarize(yr_mean = mean(as.numeric(`ADJUSTED BASIC PAY`), na.rm = TRUE), avg_pct = mean(`pay change`, na.rm = TRUE)) %>% mutate(yr_mean_change = round(opm_percent_change(yr_mean), 5))
ggplot(data = pct_summary, aes(x = year)) +
geom_line(aes(y = avg_pct, colour = "1")) +
geom_line(aes(y = yr_mean_change, color = "2")) +
geom_line(data = (opm_inflation_data_dec %>% filter(year > 1973 & year < 2014)), aes(x = year, y = pct_change, color = "3")) +
geom_line(data = turnover, aes(y = pct, color = "4")) +
scale_y_continuous(labels = scales::percent) +
scale_color_manual(name = "Percent Change in ", labels = c("Mean Percentage", "Mean Total", "Inflation", "Turnover"), values = c(cbPalette[6], cbPalette[4], cbPalette[7], cbPalette[1])) +
labs(y = "Percent Change", x = "Year", title = "VA Percentage Change per Year in Measurements") +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"), legend.justification = c(1, 1), legend.position = c(1, 1))
ggsave("sal_pct_changes_va.png", height = 5, width = 10)

# Mean pay by grade....inflation adjusted
local_data %>% group_by(year, GRADE) %>% summarize(avg = opm_inflation_adjust(year[1], 2016, mean(as.numeric(`ADJUSTED BASIC PAY`), na.rm = TRUE))) %>%
ggplot(aes(x = year, y = avg, color = GRADE)) +
geom_line() +
theme_minimal() +
labs(y = "Average Salary", x = "Year", title = "VA Average Salary by Grade") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("sal_by_grade_va.png", height = 5, width = 10)

# VA Doctor salary increases
local_data %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% filter(`PAY PLAN` == "MEDICAL AND DENTAL" | GRADE == "PH" | GRADE=="DE") %>% pull(`PSEUDO-ID`))) %>%
group_by(`PSEUDO-ID`, `group-id`) %>%
mutate(id = paste(`PSEUDO-ID`, `group-id`, sep = "-"), base = `2016 adjusted pay` - first(`2016 adjusted pay`)) %>%
ggplot(aes(x = idx - 1, y = base)) +
geom_line(aes(group = id), color = cbPalette[6], alpha = 1 / 4) +
theme_minimal() +
scale_y_continuous(labels = scales::comma) +
#scale_color_manual(values = c(cbPalette[6], cbPalette[2])) +
labs(y = "Salary Increase in 2016$", x = "Year of Observtaion", title = "Salary Increases for VA Medical Staff") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("va_doc_trajectory.png", height = 5, width = 10)

#same but with sample
sample <- local_data %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% filter(`PAY PLAN` == "MEDICAL AND DENTAL" | GRADE=="PH" | GRADE=="DE") %>% pull(`PSEUDO-ID`) %>% sample(size = 200))) %>%
group_by(`PSEUDO-ID`, `group-id`) %>%
mutate(id = paste(`PSEUDO-ID`, `group-id`, sep = "-"), base = `2016 adjusted pay` - first(`2016 adjusted pay`))
sample %>% ggplot(aes(x = idx - 1, y = base)) +
geom_line(aes(group = id), color = cbPalette[6], alpha = 1 / 2) +
theme_minimal() +
scale_y_continuous(labels = scales::comma) +
#scale_color_manual(values = c(cbPalette[6], cbPalette[2])) +
labs(y = "Salary Increase in 2016$", x = "Year of Observtaion", title = "Salary Increases for VA Medeical Staff") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("va_doc_trajectory_sampled.png", height = 5, width = 10)

#Histogram of YOS by departing employees
ggplot(data = (los_summary %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% ungroup() %>% filter(`PAY PLAN` == "MEDICAL AND DENTAL" | GRADE == "PH" | GRADE=="DE") %>% pull(`PSEUDO-ID`))) %>% filter(end != 2013)), aes(x = end, fill = los_level)) +
geom_area(stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "VA Length of Service for Depatring Doctors and Dentists") +
scale_y_continuous(labels = scales::comma) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_depart_summary_hist_ph.png", height = 5, width = 10)

# Nurses sampled, there are a ton so doing them all is not fesiable.
sample <- local_data %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% filter(grepl("NURSE", OCCUPATION)) %>% pull(`PSEUDO-ID`) %>% sample(size = 200))) %>%
group_by(`PSEUDO-ID`, `group-id`) %>%
mutate(id = paste(`PSEUDO-ID`, `group-id`, sep = "-"), base = `2016 adjusted pay` - first(`2016 adjusted pay`))
sample %>% ggplot(aes(x = idx - 1, y = base)) +
geom_line(aes(group = id), color = cbPalette[6], alpha = 1 / 2) +
theme_minimal() +
scale_y_continuous(labels = scales::comma) +
#scale_color_manual(values = c(cbPalette[6], cbPalette[2])) +
labs(y = "Salary Increase in 2016$", x = "Year of Observtaion", title = "Salary Increases for VA Nurses") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("va_nurse_trajectory_sampled.png", height = 5, width = 10)

#Histogram of YOS by departing employees
ggplot(data = (los_summary %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% ungroup() %>% filter(grepl("NURSE", OCCUPATION)) %>% pull(`PSEUDO-ID`))) %>% filter(end != 2013)), aes(x = end, fill = los_level)) +
geom_area(stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "VA Length of Service for Depatring Nurses") +
scale_y_continuous(labels = scales::comma) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_depart_summary_hist_nurse.png", height = 5, width = 10)