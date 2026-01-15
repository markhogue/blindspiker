## Initial Submission of blindspiker

## Test environments
* passed all checks with 
* local Windows 11 and R version 4.5.2
* devtools::check_win_devel()
* devtools::check_rhub()

## R CMD check results
There were no ERRORs or WARNINGS. There was a NOTE identifying this as a new package. There was another NOTE identifying hidden github files. This appears to be a result of checking the RHUB results with `rhub::rhub_setup()` and `rhub::rhub_check()` and I added these items to .Rbuildignore.
