# OPMOperations - Operations to Generate, Filter, and Derive Data from the Dataset
#    By  Matthew Churilla (matthew.churilla@outlook.com)
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


# This will number each sequence of years in a grouping so this index can be used for a 2nd level
opm_number_year_runs <- function(x) {
    #years <- as.integer(pull(x, "year"))
    #ids <- pull(x, `PSEUDO-ID`)
    x %>% mutate(`group-id` = cumsum(ifelse(is.na(lag(year)) | (`PSEUDO-ID` == lag(`PSEUDO-ID`) & (as.integer(year) != lag(as.integer(year)) + 1)), TRUE, FALSE)))
}

# OPM Fill w/ interpolation 
# Don't use this in our final analysis.
# TIP! This should be run just after dataset is loaded or calcuated columns will be wrong.
# Duplicate years next to each other break this for some reason, make sure they are filtered.
# This will run through the dataset x, assuming it is our typcial tibble format.
# Any breaks in the year sequence of max_fill or less will be
# filled with iterpolated data.  We default to 1 becuase that is the most probable error value.
# Larger breaks will need to be resolved with other mechanisms.
# This may only work on a non-organied tibble so be wary!
opm_interpolation_generate_salary <- function(x, max_fill=1) {
    # Pre-calculate a lot of stuff
    years <- as.integer(pull(x, "year"))
    ids <- pull(x, `PSEUDO-ID`)
    row_def <- x[0,] %>% ungroup()

    gap_check <- which(ifelse(!is.na(lead(years)) & ids == lead(ids) & (years + 1 != lead(years)), TRUE, FALSE))
    print(paste("Gaps to check for filling:", length(gap_check)))
    rows <- bind_rows(row_def,lapply(gap_check, function(x2) {
        i <- match(x2, gap_check)
        if (i %% 10000 == 0)
            print(paste("At", i,",",Sys.time()))
            opm_create_interpolate_rows(idx = x2, x = x, year = years, max_fill = max_fill)
        }))
    print(paste("Number of gaps filled:", length(rows$`PSEUDO-ID`)))
    # We actually want to generate years for our data before returning.
    return(rows)
}

# Will generate rows of type x that need to be inserted to close the gap between idx and idx+1
# Don't use this in our final product
opm_create_interpolate_rows <- function(idx, x, years, max_fill) {
    if(years[idx] + max_fill < years[idx + 1] - 1) return(NULL)
    fill_range <- seq(years[idx] + 1, years[idx + 1] - 1)
    len = length(fill_range)
    fill_values = as.integer(seq(x[[idx, "ADJUSTED BASIC PAY"]], x[[idx + 1, "ADJUSTED BASIC PAY"]], length.out = len + 2))
    return(bind_rows(lapply(fill_range, function(yr) {tibble(
                    `PSEUDO-ID` = x[[idx, "PSEUDO-ID"]],
                    `FILE DATE` = as.Date(paste(yr, "-12-13", sep = "")),
                    `ADJUSTED BASIC PAY` = fill_values[yr - years[idx] + 1],
                    year = as.character(yr)
                    )

    })))
}


# This may create gaps in our data, we'll clean it up later.
opm_filter_unusable_rows <- function(x) {
    x %>%
        filter(`WORK SCHEDULE` == "FULL-TIME") %>%
    #We know all work schedules are full time so filter out the columns
       select(-`WORK SCHEDULE`) %>%
        # Ensure nothing exists outside of our target range.
       filter(`FILE DATE` > as.Date("1973-11-30") & `FILE DATE` < as.Date("2014-1-1"))
}


# This will filter out all of the rows we want to remove before analysis.
# Unfortunately this a long running process but we only run it once after all data is loaded.
# We also do this in weird way to keep R from creating 3 copies of our object.
# we are removing all PSEUDO-IDs that have more than one record in any year
# also we are removing people who have more records than years of service
opm_filter_unusable_ids <- function(x) {
    print(paste("Calculating multiple year Filters...", Sys.time()))
    yos_filter <- eval.parent(x %>% ungroup() %>% group_by(`PSEUDO-ID`, `FILE DATE`) %>%
        summarize(count = n()) %>% filter(count > 1) %>% pull(`PSEUDO-ID`))
    print(paste("Calculated multi-year objects:",length(yos_filter)))
    print(paste("Calculating years of service Filters...", Sys.time()))
    yos2_filter <- eval.parent(x %>% summarize(count = n(), years = (as.integer(last(year)) + 1 - as.integer(first(year)))) %>% filter(count > years) %>% pull(`PSEUDO-ID`))
    ids <- eval.parent(x$`PSEUDO-ID`)
    print(paste("Calculated yos objects: ",length(yos2_filter)))
    #x <- x %>% ungroup() %>% group_by(`PSEUDO-ID`)
    gc()
    id_filter <- !(ids %in% unique(c(yos_filter, yos2_filter)))
    print(paste("Calculated Remaining Rows: ", sum(id_filter)))
    rm(yos_filter, yos2_filter)
    gc()
    print(paste("Filtering...", Sys.time()))
    eval.parent(return(x[id_filter,]))    
}

# This will do the following
# Group the result set by PSEUDO-ID, group-id to analize based on career segments
# Add 2016 Base Year Salary
# Add % salary change
# Add % inflation change
opm_add_calculations <- function(x) {
    x %>%
        group_by(`PSEUDO-ID`, `group-id`) %>%
    #mutate(`2016 adjusted pay` = as.integer(opm_inflation_adjust(`year`, "2016", `ADJUSTED BASIC PAY`))) %>%
    mutate(`pay change` = opm_percent_change(`ADJUSTED BASIC PAY`),
        `inflation` = opm_percent_change_inflation(`year`), idx = 1 + year - min(year, na.rm = TRUE),
        `2016 adjusted pay` = as.integer(opm_inflation_adjust(`year`, "2016", `ADJUSTED BASIC PAY`))) # %>%
        #mutate(`inflation` = opm_percent_change_inflation(`year`))
}

# Add years convience data to tibble, breaking out becuase we need it early.
opm_add_years <- function(x) {
    x %>%
        #group_by(`PSEUDO-ID`) %>%
        mutate(`year` = format(`FILE DATE`, "%Y"))
}

#Returns percent change in v2 if v1 is sequencial, otherwise NA for value
opm_ifseq_percent_change <- function(v1, v2) {
    ifelse(lag(v1) + 1 == v1, opm_percent_change(v2), NA)
}

# This has to be a builtin somewhere right?
# Calculates the percentage change between a list item and the previous item
opm_percent_change <- function(x) {
    old <- lag(x)
    new <- x
    ((new - old) / old)
}


# WARNING: This code makes assumptions about filenames and works on fixed width files
# This will iterate over all of the files in dataset assuming December data is correct yearly values
# It will pull values out of March, June, and September that do not last a year then combine and unique
# To ensure one record per year.
# Lastly it will merge with the december values to produce one record per PSEUDO-ID per year.
# Data will be organized(factored & shrunk) becuase it saves a lot of memory
opm_generate_dataset_from_fwfs <- function(files, sctdata, columns = NULL) {

    # Parse march, june, and september files keep anyhting less than a year
    result_march <- opm_load_filelist(files[endsWith(files, "_03.txt")], sctdata = sctdata, columns = columns) %>% group_by(`PSEUDO-ID`) %>% filter(n() == 1) %>% ungroup()
    result_june <- opm_load_filelist(files[endsWith(files, "_06.txt")], sctdata = sctdata, columns = columns) %>% group_by(`PSEUDO-ID`) %>% filter(n() == 1) %>% ungroup()
    result_sept <- opm_load_filelist(files[endsWith(files, "_09.txt")], sctdata = sctdata, columns = columns) %>% group_by(`PSEUDO-ID`) %>% filter(n() == 1) %>% ungroup()
    # We can now we combine these sets and use duplicated to remove all but the last
    # instance of a PSEUDO-ID
    combined <- list(result_march, result_june, result_sept) %>% bind_rows()
    combined <- combined[!duplicated(combined$`PSEUDO-ID`, fromLast = TRUE),]
    # Cleanup for upcoming big operation
    rm(result_march, result_june, result_sept)
    gc()
    # Load our dec based items only once
    results <- opm_load_filelist(files[endsWith(files, "_12.txt")], sctdata, columns = columns)
    # Merge dec dataset with items that only appear in one year so far
    # Then remove items that are in results & combined from combined
    combined <- combined[!(combined$`PSEUDO-ID` %in% results$`PSEUDO-ID`),]
    # Then merge the two datasets to get a final list of all employees over the span
    # Sort it and return
    results <- list(combined, results) %>% bind_rows() %>% arrange(`PSEUDO-ID`, `FILE DATE`)
    rm(combined)
    gc()
    return(results)
}


# This will parse and organize all of the files in pathList.
# Turns out if we don't filter columns we run out of memory with just one set.
# So use parameter columns to specify a vector of columns to keep.
# Also have ids as a list of ids to filter by, so we can grep across a lot of files for an ID
opm_load_filelist <- function(pathList, sctdata, columns = NULL, ids = NULL) {
    lapply(pathList, opm_load_and_organize, sctdata = sctdata, columns = columns, ids = ids) %>% bind_rows()
}

# Needs path to file
# Needs sctlookup data
# Optional list of columns to filter by
# Optional vector of PSEUDO-IDs to filter by
# silent doesn't print anything to console.
opm_load_and_organize <- function(path, sctdata, columns = NULL, ids = NULL, silent = FALSE) {
    result <- opm_parse_fwf(path) %>% opm_organize_tibble(sctdata)
    if (is.vector(columns)) {
        result <- result %>% select(one_of(columns))
    }
    if (is.vector(ids)) {
        result <- result %>% filter(`PSEUDO-ID` %in% ids)
    }
    if (!silent) {
        print(paste("Loaded:", path))
    }
    return(result)
}

# This will generate a tibble format that we run a model off of.
# This assumes it is being passed our first stage parsing format.
opm_generate_model_tibble <- function(x) {
    x %>% ungroup() %>% 
    # Convert grade to a factor
    mutate(GRADE = factor(GRADE), year = as.integer(year)) %>%
    # Add our derived statistics to the final format.
    opm_add_calculations() 
}


