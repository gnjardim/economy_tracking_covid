# economy_tracking_covid
Measuring and tracking Brazil's pandemic economy using mobility and energy data

## Content
- `README.md` this file, a general overview of the repository in markdown format.  
- `/code` contains all the scripts used to clean, manipulate and build the database.
- `/reports` contains all .Rmd files for generating the reports, and correspondent output files.

### Main scripts
- `code/_proc_functions.R` has all functions needed for manipulating data.
- `code/_plot_functions.R` has all functions needed for plotting data.
- `code/0_pre_processing.R` prepares data for reports.

### Shiny app
- `shiny/data` contains all files needed for running the app.
- `shiny/app.R` contains the ui and server code.