##### EEMB 144L Intro to R with CAL FIRE data ########
#Kai Oda
#10/5/2020 

# Load the tidyverse and readxl package
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)

## Load the sample fire dataset 
## NOTE: .xlsx files can have multiple sheets
##C:\Users\kaiod\Desktop\Development\EEMB_144L\144l_students\Input_Data\week1
excel_sheets("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx")

# Returns a list of the two sheets in the .xlsx file (Metadata and Data) 
calfire.metadata <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Metadata")

calfire.data  <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Data")

##### Initial Data Exploration ######
names(calfire.data) #shows the variable (column) names 

#Find out how big the dataset is. Returns (rows,columns)
dim(calfire.data)111

class(calfire.data) ## Gives the data type of the variable in question 
head(calfire.data) ## First six observations in dataset 
tail(calfire.data) ## Print out last six lines in dataset 

?names # single ? brings up R doc for that function 
??names # double ?? brings up every function that might contain names 

#Single columns referred to with the '$' 
county <- calfire.data$County_Unit

max_acres <- max(calfire.data$Total_Acres_Burned, na.rm=TRUE)
max_struc_destroyed <- max(calfire.data$Structures_Destroyed, na.rm=TRUE)

###### Basic data wrangling (dplyr functions) ######
df1 <- select(calfire.data, County_Unit:Controlled_Date, Total_Acres_Burned: Civil_Fatalities)

df2 <- filter(df1, County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "ORANGE", "SAN DIEGO") & Total_Acres_Burned >= 500 | df1$Fire_Name == "THOMAS")

df3 <- arrange(df2, desc(Start_Date), Total_Acres_Burned)
df4 <- mutate_at(df3, vars(Structures_Destroyed:Civil_Fatalities), replace_na, 0) 
df5 <- mutate(df4, Fatalities = Fire_Fatalities + Civil_Fatalities)

df6 <- mutate(df5, interval = interval(Start_Date, Controlled_Date), dur = as.duration(interval), days = as.numeric(dur, "days"))

######## Intro to piping #######
#We want to restrict our data to the SoCAl coast, exclude fires that burned less than 500 acres, add column that sums the number of fatalities, change NAs to 0s, arrange data 

socal_fires <- calfire.data %>%  mutate(Fatalities = Fire_Fatalities + Civil_Fatalities) %>%  
  mutate_at(vars(Structures_Destroyed:Civil_Fatalities), replace_na, 0)  %>% 
  mutate(interval = interval(Start_Date, Controlled_Date), dur = as.duration(interval), days = as.numeric(dur, "days")) %>%
  filter(County_Unit %in%  c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "ORANGE", "SAN DIEGO")& Total_Acres_Burned >= 500 | Fire_Name == "THOMAS") %>% 
  arrange(desc(Start_Date), Total_Acres_Burned) %>% mutate(County_Unit = ifelse(County_Unit == "VENTURA/SANTA BARBARA", "VENTURA", County_Unit))

###### Ggplot2 madness ###### 
socal.plot <- socal_fires %>% 
  rename(start=Start_Date, acres = Total_Acres_Burned,
         county = County_Unit) %>% 
  ggplot(aes(x=start, y=acres)) +
  geom_point(aes(color=county))+ 
  ggtitle("California SoCal Major Fires 2013-2018")+
  xlab("Start Date")+
  ylab("Acres burned")+
  theme(panel.grid.major = element_blank())


socal.plot + facet_grid(~county)

## 1 hour, 43 minutes into recording, cheat sheet                                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                      




