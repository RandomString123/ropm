# OPMFileTransform - Translate and factorize text fields
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

library(forcats)
# A set of methods for transforming OPM Data for Analysis
# Define the factors we will use, these are just pre-generated from the source data.
OPMAgeFactorLevels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+")
OPMLOSFactorLevels = c("< 1", "1-2", "3-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35+")

# Changes a vector to a factor based on age ranges
opm_factor_age <- function(.v) {
    factor(.v, levels = OPMAgeFactorLevels)
}

# Changes a vector to a factor based on los ranges
opm_factor_los <- function(.v) {
    factor(.v, levels = OPMLOSFactorLevels)
}

# Converts nsftp vector to a logical
opm_indicator_nsftp <- function(.v) {
    as.logical(ifelse(.v == "2", 0, 1))
}

#Takes a tibble, assumed to have OPM fields in it, and cleans it up for data analysis
opm_organize_tibble <- function(filedata, sctdata) {
    filedata %>%
    # Put in factors for Lenth of Serivce and Age
    mutate(AGE = opm_factor_age(AGE)) %>%
    mutate(`LOS LEVEL` = opm_factor_los(`LOS LEVEL`)) %>%
    # Change NSFTP to an an indicator
    mutate(NSFTP = opm_indicator_nsftp(`NSFTP IND`)) %>%
    # Hard work!  Change lookup fields into the sctfile into factors
    left_join(opm_sct_get_workschedules(sctdata), by = c("WORK SCHEDULE" = "DATA CODE")) %>%
    mutate(`WORK SCHEDULE` = factor(`WORK SCHEDULE.y`, opm_sct_get_workschedules_values(sctdata))) %>%
    select(-`WORK SCHEDULE.y`) %>%
    left_join(opm_sct_get_appointments(sctdata), by = c("TYPE OF APPOINTMENT" = "DATA CODE")) %>%
    mutate(`TYPE OF APPOINTMENT` = factor(`APPOINTMENT`, opm_sct_get_appointments_values(sctdata))) %>%
    select(-`APPOINTMENT`) %>%
    left_join(opm_sct_get_supervisorystatus(sctdata), by = c("SUPERVISORY STATUS" = "DATA CODE")) %>%
    mutate(`SUPERVISORY STATUS` = factor(`SUPERVISOR`, opm_sct_get_supervisorystatus_values(sctdata))) %>%
    select(-`SUPERVISOR`) %>%
    left_join(opm_sct_get_occupationalcategories(sctdata), by = c("OCCUPATIONAL CATEGORY" = "DATA CODE")) %>%
    mutate(`OCCUPATIONAL CATEGORY` = factor(`OCCUPATIONAL CATEGORY.y`, opm_sct_get_occupationalcategories_values(sctdata))) %>%
    select(-`OCCUPATIONAL CATEGORY.y`) %>%
    left_join(opm_sct_get_occupations(sctdata), by = c("OCCUPATION" = "DATA CODE")) %>%
    mutate(`OCCUPATION` = factor(`OCCUPATION.y`, opm_sct_get_occupations_values(sctdata))) %>%
    select(-`OCCUPATION.y`) %>%
    left_join(opm_sct_get_payplans(sctdata), by = c("PAY PLAN" = "DATA CODE")) %>%
    mutate(`PAY PLAN` = factor(`PAY PLAN.y`, opm_sct_get_payplans_values(sctdata))) %>%
    select(-`PAY PLAN.y`) %>%
    left_join(opm_sct_get_eductionlevels(sctdata), by = c("EDUCATION LEVEL" = "DATA CODE")) %>%
    mutate(`EDUCATION LEVEL` = factor(`EDUCATION LEVEL.y`, opm_sct_get_eductionlevels_values(sctdata))) %>%
    select(-`EDUCATION LEVEL.y`) %>%
    # Duty stations seems to break code parsing, guessing a bad character in one of the names.
    # TODO: try this again, think it was a code bug in another section overwriting a name.
    #left_join(opm_sct_get_dutystations(sctdata), by = c("DUTY STATION" = "DATA CODE")) %>%
    #mutate(`DUTY STATION` = factor(`DUTY STATION.y`, opm_sct_get_dutystations_values(sctdata))) #%>%
    #select(-`DUTY STATION.y`) %>%
    left_join(opm_sct_get_agencies(sctdata), by = c("AGENCY" = "DATA CODE")) %>%
    mutate(`AGENCY` = factor(`AGENCY.y`, opm_sct_get_agencies_values(sctdata))) %>%
    select(-`AGENCY.y`)
}

# Will pull all values for a table ID out of tibble parameter sctfile and return them
# sctfile tibble of data to pull from
# tblanme two character code of table to pull
opm_sct_get_table <- function(sctfile, tblname) {
    sctfile %>%
        filter(`SCT TABLE ID` == tblname) %>%
        select(c("DATA CODE", "TRANSLATION 1"))
}

#Pulls appointments "VM" out of the SCT DATA, assums SCT structure of tibble
opm_sct_get_appointments <- function(sctfile) {
    opm_sct_get_table(sctfile, "VM") %>% rename("APPOINTMENT" = `TRANSLATION 1`)
}

opm_sct_get_appointments_values <- function(sctfile) {
    opm_sct_get_appointments(sctfile) %>% pull("APPOINTMENT")
}

opm_sct_get_eductionlevels <- function(sctfile) {
    opm_sct_get_table(sctfile, "EV") %>% rename("EDUCATION LEVEL" = `TRANSLATION 1`)
}

opm_sct_get_eductionlevels_values <- function(sctfile) {
    opm_sct_get_eductionlevels(sctfile) %>% pull("EDUCATION LEVEL")
}

opm_sct_get_occupationalcategories <- function(sctfile) {
    opm_sct_get_table(sctfile, "GF") %>% rename("OCCUPATIONAL CATEGORY" = `TRANSLATION 1`)
}

opm_sct_get_occupationalcategories_values <- function(sctfile) {
    opm_sct_get_occupationalcategories(sctfile) %>% pull("OCCUPATIONAL CATEGORY")
}

opm_sct_get_payplans <- function(sctfile) {
    opm_sct_get_table(sctfile, "LA") %>% rename("PAY PLAN" = `TRANSLATION 1`)
}

# There are duplicate pay plan names for codes, executive decision: just remove duplicates
opm_sct_get_payplans_values <- function(sctfile) {
    opm_sct_get_payplans(sctfile) %>% pull("PAY PLAN") %>% unique()
}

opm_sct_get_supervisorystatus <- function(sctfile) {
    opm_sct_get_table(sctfile, "SU") %>% rename("SUPERVISOR" = `TRANSLATION 1`)
}

opm_sct_get_supervisorystatus_values <- function(sctfile) {
    opm_sct_get_supervisorystatus(sctfile) %>% pull("SUPERVISOR")
}

opm_sct_get_workschedules <- function(sctfile) {
    opm_sct_get_table(sctfile, "WS") %>% rename("WORK SCHEDULE" = `TRANSLATION 1`)
}

opm_sct_get_workschedules_values <- function(sctfile) {
    opm_sct_get_workschedules(sctfile) %>% pull("WORK SCHEDULE")
}

opm_sct_get_occupations <- function(sctfile) {
    opm_sct_get_table(sctfile, "XB") %>% rename("OCCUPATION" = `TRANSLATION 1`)
}

# There are duplicate occupation names for codes, executive decision: just remove duplicates
opm_sct_get_occupations_values <- function(sctfile) {
    opm_sct_get_occupations(sctfile) %>% pull("OCCUPATION") %>% unique()
}

opm_sct_get_dutystations <- function(sctfile) {
    opm_sct_get_table(sctfile, "VX") %>% rename("DUTY STATION" = `TRANSLATION 1`)
}

opm_sct_get_dutystations_values <- function(sctfile) {
    opm_sct_get_dutystations(sctfile) %>% pull("DUTY STATION")
}

# Agencies is a bit tricker than others, it has duplicates so we need to de,dupe it in a most likely not correct manner
opm_sct_get_agencies <- function(sctfile) {
    tmp <- opm_sct_get_table(sctfile, "AH") %>% rename("AGENCY" = `TRANSLATION 1`)
    tmp[!duplicated(tmp$`DATA CODE`, fromLast=TRUE),]
}

opm_sct_get_agencies_values <- function(sctfile) {
    opm_sct_get_agencies(sctfile) %>% pull("AGENCY") %>% unique()
}

# THESE ARE NOT SALARY GRADES, NOT SURE WHAT THEY ARE
# Figured it out...they are combo codes for combined plan+grade lookup
opm_sct_get_grades <- function(sctfile) {
    opm_sct_get_table(sctfile, "VK") %>% rename("GRADE" = `TRANSLATION 1`)
}

opm_sct_get_dutystations_values <- function(sctfile) {
    opm_sct_get_grades(sctfile) %>% pull("GRADE")
}
