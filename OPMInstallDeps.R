# OPMInstallDeps - Load / Install depencies for rest of the files to operate
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

if (install) { 
    install.packages("dplyr") #repos = "http://mran.revolutionanalytics.com")
    install.packages("tidyverse") #repos = "http://mran.revolutionanalytics.com")
    install.packages("reshape2") #repos = "http://mran.revolutionanalytics.com")
}

source("OPMFileParsing.R")
source("OPMFileTransform.R")
source("OPMOperations.R")
# Keep this last
source("OPMExternalData.R")
