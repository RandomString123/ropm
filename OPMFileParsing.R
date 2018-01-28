# OPMFileParsing - Basic Static Text File Loader / Parser
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

library(tidyverse)

# A huge list of NA values that exist in the files, pretty much fields filled with * # Or special strings.
# NAME WITHHELD usualy is in name
# UNSP is found in Ages
# <NA> appears in duty station
# 000000, 999999 - These signify speical values in the SCTFILE but parse horribly as dates
# "" is in type of appointment
vOPMNaValues = c(strrep("*", 1:23), strrep("#", 1:23), "NAME WITHHELD BY AGENCY", "NAME WITHHELD BY OPM", "NAME UNKNOWN", "UNSP", "<NA>", "000000", "999999", "")

#Weird data found...
#Pay Grade CM/PH - This is a combined grade you can lookup in tables...TODO

# Fixed width file parser
# Files Have the following format...
# Except early files don't have column 7(years since degree)
# Title,Start,End,Length
# PSEUDO-ID,1,9,9
# EMPLOYEE NAME,10,32,23
# FILE DATE,33,40,8
# AGENCY,41,44,4
# DUTY STATION,45,53,9
# AGE,54,59,6
# YEARS SINCE DEGREE,60,65,6 -- Not in early files need to adjust
# EDUCATION LEVEL,66,67,2
# PAY PLAN,68,69,2
# GRADE,70,71,2
# LOS LEVEL,72,77,6
# OCCUPATION,78,81,4
# OCCUPATIONAL CATEGORY,82,82,1
# ADJUSTED BASIC PAY,83,88,6
# SUPERVISORY STATUS,89,89,1
# TYPE OF APPOINTMENT,90,91,2
# WORK SCHEDULE,92,92,1
# NSFTP IND,93,93,1

# A format for the CPDF Standard Code Format
# SCTFILE.TXT has a bad last line, remove it before processing.
CPDFStandardFormat <- frame_data(
    ~FieldName,~Length, ~Type,
    "SCT TABLE ID", 2, col_character(),
    "DATA CODE",9, col_character(),
    "CODE USE FROM 1", 6, col_date(format="%Y%m"),
    "CODE USE UNTIL 1", 6, col_date(format = "%Y%m"),
    "CODE USE FROM 2", 6, col_date(format="%Y%m"),
    "CODE USE UNTIL 2", 6, col_date(format="%Y%m"),
    "TRANSLATION 1", 40, col_character(),
    "TRANSLATION IN USE FROM 1", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 1", 6, col_date(format="%Y%m"),
    "TRANSLATION 2", 40, col_character(),
    "TRANSLATION IN USE FROM 2", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 2", 6, col_date(format="%Y%m"),
    "TRANSLATION 3", 40, col_character(),
    "TRANSLATION IN USE FROM 3", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 3", 6, col_date(format="%Y%m"),
    "TRANSLATION 4", 40, col_character(),
    "TRANSLATION IN USE FROM 4", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 4", 6, col_date(format="%Y%m"),
    "TRANSLATION 5", 40, col_character(),
    "TRANSLATION IN USE FROM 5", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 5", 6, col_date(format="%Y%m"),
    "TRANSLATION 6", 40, col_character(),
    "TRANSLATION IN USE FROM 6", 6, col_date(format="%Y%m"),
    "TRANSLATION IN USE UNTIL 6", 6, col_date(format="%Y%m")
    )

# We need a tibble for our previous defination...hardcoding this inline for now.
OPMFedFomat <- frame_data(
    ~FieldName,~Length,~Type,
    "PSEUDO-ID",9,col_integer(),
    "EMPLOYEE NAME",23,col_character(),
    "FILE DATE",8, col_date(format="%Y%m%d"),
    "AGENCY",2,col_character(),
    "SUB AGENCY",2,col_character(),
    "DUTY STATION",9,col_character(),
    "AGE",6,col_character(),
    "YEARS SINCE DEGREE",6,col_number(),
    "EDUCATION LEVEL",2,col_character(),
    "PAY PLAN",2,col_character(),
    "GRADE",2,col_character(),
    "LOS LEVEL",6,col_character(),
    "OCCUPATION",4,col_character(),
    "OCCUPATIONAL CATEGORY",1,col_character(),
    "ADJUSTED BASIC PAY",6,col_integer(),
    "SUPERVISORY STATUS",1,col_character(),
    "TYPE OF APPOINTMENT",2,col_character(),
    "WORK SCHEDULE",1,col_character(),
    "NSFTP IND", 1,col_character()
    )

# This function will parse and return a tibble of a file with the assumption it is a FedScope fixed width 
# file.  Results will be unknown if the file is not a FedScope fixed width file.
# This function also has the ability to expand collapsed fields using helper functions.
# Parameters:
# path: The path to the file that will be read.
# post2014: boolean flag that signals the file is after 2014 and thus contains "Years Since Degree"
# incYearsSinceDegree: Will include the years since degree column as NA if not present.
opm_parse_fwf <- function(path, post2014 = FALSE, incYearsSinceDegree = TRUE) {
    if (post2014)
        data <- OPMFedFomat
    else
        data <- filter(OPMFedFomat, FieldName != "YEARS SINCE DEGREE")
    colsObj <- mapply(function(x, y) { y }, pull(data, FieldName), pull(data, Type), SIMPLIFY = FALSE, USE.NAMES = TRUE)
    result <- read_fwf(path, fwf_widths(pull(data, Length), pull(data, FieldName)), colsObj, na = vOPMNaValues,progress = !interactive())
    if (!post2014 && incYearsSinceDegree)
        result <- result %>% add_column(`YEARS SINCE DEGREE` = NA, .after = "AGE") %>% mutate(`YEARS SINCE DEGREE` = as.integer(`YEARS SINCE DEGREE`))
    return(result)
}

# This function will read in the cpdf format file named sctdata.  This is a static data table of 
# normalized values from the main dataset.  
opm_parse_sctfile_fwf <- function(path) {
    data <- CPDFStandardFormat
    # Ugly line of code to convert a tbl to parameter list
    colsObj <- mapply(function(x, y) { y }, pull(data, FieldName), pull(data, Type), SIMPLIFY = FALSE, USE.NAMES = TRUE)
    result = read_fwf(path, fwf_widths(pull(data, Length), pull(data, FieldName)), colsObj, na = vOPMNaValues, progress = !interactive())
    return(result)
}
