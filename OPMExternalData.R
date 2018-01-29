# OPMExternalData - Inflation and Presidential Data and Operations
#    By 2017  Matthew Churilla (matthew.churilla@outlook.com)
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

# Rebase an amount of money in a given year to December 2016 dollars
opm_inflation_adjust <- function(year, base_year, amt) {
    base <- opm_get_inflation_index(base_year)
    div <- sapply(year, function(x) { opm_get_inflation_index(x) })
    amt * (base / div)
}

# We store the index in a hash for fast access, so breaking this out into a function
opm_get_inflation_index <- function(year) {
    tmp <- as.character(year)
    sapply(tmp, function(x) { ifelse(is.na(x), NA, get(x, envir = opm_inflation_hash)) })
}

# Takes a vector of years, gives percentage change between an element and the one before
opm_percent_change_inflation <- function(years) {
    inflation <- opm_get_inflation_index(years)
    opm_percent_change(inflation)
}

# december of given year inflation index numbers.
opm_inflation_data_dec <- frame_data(
    ~year, ~ index,
    1970, 39.8,
    1971, 41.1,
    1972, 42.5,
    1973, 46.2,
    1974, 51.9,
    1975, 55.5,
    1976, 58.2,
    1977, 62.1,
    1978, 67.7,
    1979, 76.7,
    1980, 86.3,
    1981, 94.0,
    1982, 97.6,
    1983, 101.3,
    1984, 105.3,
    1985, 109.3,
    1986, 110.5,
    1987, 115.4,
    1988, 120.5,
    1989, 126.1,
    1990, 133.8,
    1991, 137.9,
    1992, 141.9,
    1993, 145.8,
    1994, 149.7,
    1995, 153.5,
    1996, 158.6,
    1997, 161.3,
    1998, 163.9,
    1999, 168.3,
    2000, 174.0,
    2001, 176.7,
    2002, 180.9,
    2003, 184.3,
    2004, 190.3,
    2005, 196.8,
    2006, 201.8,
    2007, 210.036,
    2008, 210.228,
    2009, 215.949,
    2010, 219.179,
    2011, 225.672,
    2012, 229.601,
    2013, 233.049,
    2014, 234.812,
    2015, 236.525,
    2016, 241.432
)

# Build a hash of our inflation data becuase we do a lot of lookups into it and table filtering
# is really slow for that many iterations.
opm_inflation_hash <- new.env(hash = TRUE)
invisible(apply(opm_inflation_data_dec, 1, function(x) { assign(as.character(x['year']), x['index'], envir = opm_inflation_hash) }))
opm_inflation_data_dec <- opm_inflation_data_dec %>% mutate(pct_change = opm_percent_change(index))

# For lack of a better place, putting this here.
opm_presidental_transition_years = c(1974, 1977, 1981, 1989, 1993, 2001, 2009)
opm_presidential_names = c("Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama")
opm_presidents_tibble = tibble(years = as.integer(opm_presidental_transition_years), names = opm_presidential_names)