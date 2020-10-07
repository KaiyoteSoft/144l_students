##### EEMB 144L Intro to R with CAL FIRE data ########
#Kai Oda
#10/5/2020 

# Load the tidyverse and readxl package
library(tidyverse)
library(readxl)

## Load the sample fire dataset 
## NOTE: .xlsx files can have multiple sheets 
excel_sheets("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx")

# Returns a list of the two sheets in the .xlsx file (Metadata and Data) 
calfire.metadata <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Metadata")

calfire.data  <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Data")

## git test 
