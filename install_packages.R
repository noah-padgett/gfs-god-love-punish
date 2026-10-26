# Script: install_packages.R
# Created by: R. Noah Padgett & Chris Felton
# Last edited on: 2024-05-12

# WARNING: The package was set up to be as user-friendly as possible for researchers
#	part of the GFS core team who mainly have experience with other statistical analysis
#	software such as STATA, SAS, and SPSS.
#	This package and implementation of the analyses for Wave 2 of the Global Flourishing
#	Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

install.packages("remotes")
remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)

# installs most needed packages
# may need to run more than once as shown here: (INSERT YOUTUE LINK)
Rglobalflourishing::load_packages()

# install LaTeX
install.packages('tinytex')
tinytex::install_tinytex(TRUE)
tinytex::tlmgr_install(c('opensans'))
