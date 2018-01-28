# OPMSEC - Charts and Graphs for the Securites and Exchange Comission
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
local_data <- model %>% ungroup() %>% filter(AGENCY == "SECURITIES AND EXCHANGE COMMISSION")

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
labs(y = "Number of Full Time Employees" , x = "Year", fill = "Length of Service", title = "SEC Length of Service Over Time") +
#scale_y_continuous(labels = scales::percent) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_summary_hist_sec.png", height = 5, width = 10)

pct_summary <- local_data %>% ungroup() %>% group_by(year) %>% summarize(yr_mean = mean(as.numeric(`ADJUSTED BASIC PAY`), na.rm = TRUE), avg_pct = mean(`pay change`, na.rm = TRUE)) %>% mutate(yr_mean_change =  round(opm_percent_change(yr_mean), 5))
ggplot(data = pct_summary, aes(x = year)) +
geom_line(aes(y = avg_pct, colour = "1")) +
geom_line(aes(y = yr_mean_change, color = "2")) +
geom_line(data = (opm_inflation_data_dec %>% filter(year > 1973 & year < 2014)), aes(x = year, y = pct_change, color = "3")) +
geom_line(data = turnover, aes(y = pct, color = "4")) +
scale_y_continuous(labels = scales::percent) +
scale_color_manual(name = "Percent Change in ", labels = c("Mean Percentage", "Mean Total", "Inflation", "Turnover"), values = c(cbPalette[6], cbPalette[4], cbPalette[7], cbPalette[1])) +
labs(y = "Percent Change", x = "Year", title = "SEC Percentage Change per Year in Measurements") +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"), legend.justification = c(1, 1), legend.position = c(1, 1))
ggsave("sal_pct_changes_sec.png", height = 5, width = 10)

# Mean pay by grade....inflation adjusted
local_data %>% group_by(year, GRADE) %>% summarize(avg = opm_inflation_adjust(year[1], 2016, mean(as.numeric(`ADJUSTED BASIC PAY`), na.rm = TRUE))) %>%
ggplot(aes(x = year, y = avg, color = GRADE)) +
geom_line() +
theme_minimal() +
labs(y = "Mean Salary 2016$", x = "Year", title = "Mean Salary by Grade") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("sal_by_grade_sec.png", height = 5, width = 10)

# Filter less than 300,000 for appearent data error of bindery guy making 376k in 1986 (id=8605011)
# Some people ~10 start at idx > 1 becuase they transfer in
# -1 to idx for people like me who think charts should start at the origin.
chart_data <- local_data %>% ungroup() %>% filter(`PSEUDO-ID` %in% (local_data %>% ungroup() %>% filter(`PAY PLAN` == "SEC EMP FORMERLY UNDER ES PAY PLAN") %>% pull(`PSEUDO-ID`) %>% unique())) %>%
group_by(`PSEUDO-ID`, `group-id`) %>% mutate(id = paste(`PSEUDO-ID`, `group-id`, sep = "-"), base = `2016 adjusted pay` - first(`2016 adjusted pay`), pre = year < 2002) %>% filter(base < 300000)
chart_data %>%
ggplot(aes(x = idx - 1, y = base)) +
geom_line(aes(group = id, color = pre), alpha = 1/5) +
geom_smooth(aes(group = pre, color = pre), se = FALSE) +
theme_minimal() +
scale_color_manual(name="Pre Pay Parity", values = c(cbPalette[6], cbPalette[2])) +
labs(y = "Salary Change in 2016$", x = "Year of Observtaion", title = "SEC Salary Increases for Pay Parity Employees") +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("es_trajectory.png", height = 5, width = 10)

#Histogram of YOS by departing employees
ggplot(data = (los_summary %>% filter(end != 2013)), aes(x = end, fill = los_level)) +
geom_area(stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "SEC Length of Service for Depatring Employees") +
#scale_y_continuous(labels = scales::percent) +
scale_y_continuous(labels = scales::comma) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_depart_summary_hist_sec.png", height = 5, width = 10)

