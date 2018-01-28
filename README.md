# R code for OPM Salary Data analysis

BuzzFeed performed an FOA request and got fourty years of federal employment data.  This project is R
code that will read in, tidy, and mutate the fixed with files provided in this dataset into an 
R data source file which is faster and easier to work with than the provided fixed width files.  This project also contains code that does analyis can charting of a portion of the data.

This code is posted for personal use, but public in the event it will help someone else looking to explore this dataset.

Data Source: https://archive.org/details/opm-federal-employment-data

# Basic Information

The attached zip file contains the R code used to generate my dataset and graphs.  

* OPMChartsAndGraphs.R – Contains code for generating all of the general charts in graphs
* OPMEntryPoint.R- Contains entry point to generate dataset from source files
* OPMExternalData.R- Contains BLS and Presidential Data
* OPMFileParsing.R- Methods for parsing OPM flat text files
* OPMFileTransform.R- Methods to transform file values to translated values and factor data
* OPMFileInstallDeps.R- Installs all necessary dependencies
* OPMOperations.R- Operations used to create calculated fields and filter data
* OPMSEC.R- Contains code to generate SEC charts 
* OPMVA.R- Contains code to generate VA charts

# For Just Parsing

If someone is coming to this project just looking for code that parses the file and converts all of its columns to factors then I suggest starting with the function opm_load_and_organize from OPMOperations.  This function takes in a list of the files the user would like to parse, the translation file, columns of interest, rows of interest and outputs a data frame that contains tidy data from all of the source files.  

# Creating the dataset how I used it

When authoring the dataset creation code, I had to work around the limited nature of the machine I was using for analysis (A Surface Pro with 8GB of memory).  I am confident this code will successfully execute on a machine with this little memory but it may fail after each major step, due to R fragmenting memory, so a machine with more memory will require less reloads to be successful.  The machine this process is run on also needs 50GB of disk space for the full text file dataset and the derived R data sources and charts.  

To re-create the dataset these steps should be taken:

1.	Download and extract the text files from here: https://archive.org/details/opm-federal-employment-data
2.	Extract the R source files from the accompanying zip file
3.	Edit OPMEntryPoint.R, the file contains constants used in parsing that need to be set:
    1.	sctfile – should be the full path and filename to SCTFILE.TXT in the dataset
    2.	fileroot – should be the path to the root of the 1973 to 2014 dataset
4.	Start R
5.	Set the current working directory to where the source files were extracted.
6.	Source OPMEntryPoint.R, this code will take 1-2 hours to execute but it may fail after each major step with an out of memory error on machines with low memory.  If this happens just restart R to reduce the memory fragmentation and re-execute the file, it will continue from the last major checkpoint.  Messages will be output that mark completion of major steps:
    1.	“Result file generated” – will appear when the basic results file has been generated
    2.	"Filtered file generated" – will appear when the basic results have been filtered
    3.	"Model file generated"- will appear when the final model dataset has been generated

When the string “Model file generated” is output the code has successfully parsed the text files and from then on OPMEntryPoint.R will load from an R data source and not the text files unless the variable refresh is set to TRUE.  This loading from an R data source only takes a few minutes to execute so it can be done frequently.
With the dataset created the files OPMChartsAndGraphs.R, OPMSEC.R, and OPMVA.R can be executed, either in whole or in part to generate the charts that appear in the report.  These files have a small block of summary creation code at their start which is followed by blocks of code for each chart that is generated.

