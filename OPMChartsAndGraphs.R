# OPMChartsAndGraphs - General Charts and Graphs for all dataset records
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

library(reshape2)
library(modelr)
cbPalette <- c("#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Calculate summary statistics for our charts
#Ensure result is sorted by PSEUDO then GROUP ID
# How long each employment segment is and length between employment segments for those with more than one.
los_summary <- model %>% group_by(`PSEUDO-ID`, `group-id`) %>% summarize(los = n(), start = as.integer(first(year)), end = as.integer(last(year)), los_level = last(`LOS LEVEL`)) %>% group_by(`PSEUDO-ID`) %>% mutate(length_between = start - lag(end) - 1)
# Number of service segments with shortest and longest recorded.
num_los_summary <- los_summary %>% group_by(`PSEUDO-ID`) %>% summarize(num = n(), shortest = min(los), longest = max(los))
# Total employed in any year
tot_by_year <- model %>% group_by(year) %>% summarize(count = n())
# Arrivals, first year of service.
arr_by_year <- los_summary %>% group_by(start) %>% summarize(starts = n())
arr_by_year[arr_by_year$start == 1973, 2] <- NA
# Departures, Year after last year of service
dep_by_year <- los_summary %>% group_by(end) %>% summarize(ends = n())
dep_by_year[, 2] <- lag(dep_by_year$ends)
# Table of count, arrivals, departures
turnover <- merge(merge(tot_by_year, arr_by_year, by.x = "year", by.y = "start"), dep_by_year, by.x = "year", by.y = "end")
turnover <- as.tibble(turnover) %>% mutate(year=as.integer(year), pct = ends / ((count + lag(count))/2))

# Total Employees and Presidential years plot
ggplot(data = tot_by_year, aes(x = as.integer(year), y = count)) +
geom_line(stat = "identity", colour = cbPalette[6], show.legend = FALSE) + 
#geom_smooth(method = "lm", se = FALSE, aes(color = cbPalette[2]), show.legend = FALSE) +
labs(y = "Number Of Fulltime Employees", x = "Year", color="num", title = "Employment Records by Year") +
coord_cartesian(ylim = c(500000, 1250000)) +
geom_vline(data = opm_presidents_tibble, aes(xintercept = years, colour = factor(years)), linetype = "solid", show.legend = FALSE) +
geom_text(data = opm_presidents_tibble, mapping = aes(x = years, y = ifelse(1:7 %% 2, 1250000, 1200000), label = names), show.legend = FALSE) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("emp_total.png", height = 5, width = 10)


# Starting / Ending Chart
ggplot(melt(select(turnover, - one_of(c("count", "pct"))), id.vars = "year")) +
geom_bar(stat = "identity", position = "dodge", aes(x = year, y = value, fill = variable)) +
scale_fill_manual(labels = c("Starting", "Ending"), values = c(cbPalette[6], cbPalette[2])) +
labs(y = "Number Of Fulltime Employees", x = "Year", fill = NULL, title = "Employees Starting and Ending Employment") +
geom_vline(data = opm_presidents_tibble, aes(xintercept = years, colour = factor(years)), linetype = "solid", show.legend = FALSE) +
geom_text(data = opm_presidents_tibble, mapping = aes(x = years, y = 200000, label = names), show.legend = FALSE) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("emp_starts_ends.png", height = 5, width = 10)
# Summary turnover/count data
summary(turnover[, 2:4])

# Length of Service histogram area chart
ggplot(data = model, aes(x = year)) +
geom_area(aes(fill = `LOS LEVEL`), stat = "bin", binwidth = 1) +
scale_fill_brewer(palette = "Spectral") +
labs(y = "Number of Full Time Employees", x = "Year", fill = "Length of Service", title = "Length of Service Over Time") +
geom_vline(data = opm_presidents_tibble, aes(xintercept = years, colour = factor(years)), linetype = "solid", show.legend = FALSE) +
geom_text(data = opm_presidents_tibble, mapping = aes(x = years, y = 1250000, label = names), show.legend = FALSE) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm")) +
ggsave("los_summary_hist.png", height = 5, width = 10)

# Summary Length of Service
summary(los_summary$los, digits = 2)

# Chart of Length of Service
ggplot(data = los_summary, aes(x = los)) +
labs(title = "Count Weighted by Number of Observations", y = "Weighted Count or Total Years", x = "Number of Observations") +
geom_bar(aes(y = ..count.. * ..x..), fill = cbPalette[6], stat = "count") +
geom_line(aes(y = ..count..), stat = "count", size = 2, colour = cbPalette[2]) +
guides(fill = FALSE) +
theme_minimal() +
scale_fill_brewer() +
scale_color_brewer() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("los_chart.png", height = 5, width = 10)

# Chart of segments > 1
num_los_summary %>% filter(num > 1) %>% ggplot(aes(x = num, colours = num)) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
    geom_text(aes(label = ..count.., y = ..prop..), stat = "count") +
    labs(y = "Percent of Total", x = "Number of Employment Segments", title="Employees With More than One Employment Segment", fill = "num") +
    scale_fill_manual(values = cbPalette) +
#facet_grid(~num) #+
scale_y_continuous(labels = scales::percent) +
#coord_fixed(ratio = 5) +
guides(fill = FALSE) +
theme_minimal() + 
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("emp_seg.png", height = 5, width = 10)

# Summary statistics for length between service
summary(los_summary$length_between[!is.na(los_summary$length_between)], digits = 2)
# Calculate the length between service
los_summary %>% ungroup() %>% filter(!is.na(length_between)) %>% ggplot(aes(x = length_between, colours = length_between)) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count", fill=cbPalette[6]) +
#geom_text(aes(label = scales::percent(..prop..)), stat = "count", vjust = -.5) +
labs(y = "Percent", x = "Years Between Positions", title="Distribution of Years Between Positions", fill = "length_between") +
#scale_fill_manual(values = cbPalette) +
#facet_grid(~num) #+
#scale_fill_manual(values = cbPalette) +
scale_y_continuous(labels = scales::percent) +
#coord_fixed(ratio = 5) +
guides(fill = FALSE) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("emp_btwn_seg.png", height = 5, width = 10)

# Number of records lacking salary numbers
sum(is.na(model$`ADJUSTED BASIC PAY`))

# Overall Average Salary
model %>% ungroup() %>% group_by(year) %>% summarize(mean = opm_inflation_adjust(year[1], 2016, mean(`ADJUSTED BASIC PAY`, na.rm = TRUE))) %>%
ggplot(aes(x = year)) +
geom_line(aes(y = mean), color = cbPalette[6]) +
#geom_smooth(aes(y = mean), se = FALSE, span = .2, color=cbPalette[6]) +
#geom_smooth(aes(y = mean2), se = FALSE, span = .2, color="green") +
geom_vline(data = opm_presidents_tibble, aes(xintercept = years, colour = factor(years)), linetype = "solid", show.legend = FALSE) +
geom_text(data = opm_presidents_tibble, mapping = aes(x = years, y = 85000, label = names), show.legend = FALSE) +
labs(y = "Average Salary in 2016$", x = "Year", title = "Average Salary For All Employees") +
scale_color_manual(values = cbPalette) +
guides(fill = FALSE) +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("sal_tot_mean.png", height = 5, width = 10)

# Compare pct increase to inflation
pct_summary <- model %>% ungroup() %>% group_by(year) %>% summarize(yr_mean = mean(as.numeric(`ADJUSTED BASIC PAY`), na.rm = TRUE), avg_pct = mean(ifelse(year == 1992, NA, `pay change`), na.rm = TRUE)) %>% mutate(yr_mean_change = round(opm_percent_change(yr_mean), 5))
ggplot(data = pct_summary, aes(x = year)) +
geom_line(aes(y = avg_pct, colour = "1")) +
geom_line(aes(y = yr_mean_change, color = "2")) +
geom_line(data = (opm_inflation_data_dec %>% filter(year > 1973 & year < 2014)), aes(x = year, y = pct_change, color = "3")) +
geom_line(data = turnover, aes(y = pct, color = "4")) +
scale_y_continuous(labels = scales::percent) +
scale_color_manual(name = "Percent Change in ", labels = c("Mean Percentage", "Mean Total", "Inflation", "Turnover"), values = c(cbPalette[6], cbPalette[4], cbPalette[7], cbPalette[1])) +
labs(y = "Percent Change", x = "Year", title = "Percentage Change per Year in Measurements") +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"), legend.justification = c(1, 1), legend.position = c(1, 1))
ggsave("sal_pct_changes.png", height = 5, width = 10)

# Create a picture of all agencies at once, we need to summarize salary by year
avg_summary <- model %>% group_by(AGENCY, year) %>% summarize(avg_2016 = as.integer(opm_inflation_adjust(year[1], 2016, mean(`ADJUSTED BASIC PAY`, na.rm = TRUE))))
avg_summary %>% ggplot() +
geom_line(aes(year, avg_2016, group = AGENCY), alpha = 1 / 4) +
geom_smooth(aes(year, avg_2016), se = FALSE, color = cbPalette[6], size = 2) +
labs(y = "Mean Total Pay in 2016$", x = "Year", title = "Mean Pay by Agency") +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("sal_agencies_mean.png", height = 5, width = 10)


# Call out our agencies.
look <- avg_summary %>% filter(AGENCY == "SECURITIES AND EXCHANGE COMMISSION")
look2 <- avg_summary %>% filter(AGENCY == "VETERANS ADMINISTRATION")
avg_summary %>% ggplot() +
geom_line(aes(year, avg_2016, group = AGENCY), alpha = 1 / 4) +
geom_smooth(aes(year, avg_2016), se = FALSE, color = cbPalette[6], size = 2) +
geom_line(data = look, aes(year, avg_2016, color = "1"), size = 2) +
geom_line(data = look2, aes(year, avg_2016, color = "2"), size = 2) +
scale_color_manual(name = "Percent Change in ", labels = c("SEC", "VA"), values = c(cbPalette[2], cbPalette[7])) +
labs(y = "Mean Total Pay in 2016$", x = "Year", title = "Mean Pay by Agency") +
theme_minimal() +
theme(plot.margin = grid::unit(c(1, 1, 1, 1), "mm"))
ggsave("sal_agencies_callout.png", height = 5, width = 10)


                   
