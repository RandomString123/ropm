# OPMEntryPoint - Load / Create a dataset from flat text files or r datasource
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

# Set this to true to force a refresh from source datafiles
refresh <- FALSE
# Set this to true to install packages from the internet
install <- TRUE
# Hardcode a location to the sctfile here.
sctfile <- "c://Users/matth/Desktop/data/1973-09-to-2014-06/SCTFILE.TXT"
# This is the location of the root of the files
fileroot <- "c://Users/matth/Desktop/data/1973-09-to-2014-06"

# Location of files of interest within our root
subdir <- "/non-dod/status"
path <- paste(fileroot, subdir, sep = "")
# Filename to write our results to disk or read from disk.
result_filename <- paste(path, "/../non-dod-summary.rds", sep = "")
filtered_filename <- paste(path, "/../non-dod-filtered.rds", sep = "")
model_filename <- paste(path, "/../non-dod-model.rds", sep="")

# List of fields to pull from the files.
fields_of_interest = c("PSEUDO-ID", "FILE DATE", "AGENCY", "OCCUPATION", "LOS LEVEL", "PAY PLAN", "GRADE", "ADJUSTED BASIC PAY", "WORK SCHEDULE")

### Executed code from here on ###
# Get our filelist and parse sctdata
files <- list.files(path, full.names = TRUE, recursive = TRUE)
# This loads all files and includes depencenies
source("OPMInstallDeps.R")
sctdata <- opm_parse_sctfile_fwf(sctfile)

# If refresh...delete our files.
if (refresh) {
    file.remove(c(result_filename, filtered_filename, model_filename))
}

# Load and prepare data...
# otherwise load from source files and sleep for a few hours.
if (!file.exists(result_filename)) {
    print(paste(Sys.time(), "No Basic Result File, Reparsing sources."))
    result <- opm_generate_dataset_from_fwfs(files, sctdata = sctdata, columns = fields_of_interest)
    write_rds(result, result_filename)
    # If we wrote this file remove the filtered file, just to be sure
    file.remove(filtered_filename)
    print(paste(Sys.time(), "Result file generated"))
}

if (!file.exists(filtered_filename)) {
    print(paste(Sys.time(), "No Filtered File, Performing filtering and grouping"))
    rm(result)
    gc()
    # This just always reads from the rds to hopefully allow the above to cleanup memory.
    result <- read_rds(result_filename) %>% ungroup() %>% opm_filter_unusable_rows() %>% opm_add_years() %>% group_by(`PSEUDO-ID`)
    # Sneak in a gc to eek out a bit more memory.
    gc()
    result <- result %>% opm_filter_unusable_ids() %>% opm_number_year_runs() %>% group_by(`PSEUDO-ID`, `group-id`)
    write_rds(result, filtered_filename)
    print(paste(Sys.time(), "Filtered file generated"))
} else if (!file.exists(model_filename)) {
    print(paste(Sys.time(), "Filtered File found.  Loading for model building."))
    result <- read_rds(filtered_filename)
}


if (!file.exists(model_filename)) {
    print(paste(Sys.time(), "Model file not found, building model"))
    model <- opm_generate_model_tibble(result)
    write_rds(model, model_filename)
    rm(result)
    gc()
    print(paste(Sys.time(), "Model file generated"))
} else {
    print(paste(Sys.time(), "Loading model file."))
    model <- read_rds(model_filename)
}
# Interesting cases to look at...
# 8,289, 21, 14, 10357722, 166
# 21 is kelli
# 8 departs after 3 years of no increases
# 166, 289, 14 fragmented employment
#
#72738 - termination + long service