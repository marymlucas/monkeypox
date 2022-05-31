# Original version at
# https://github.com/NafissahPouye/dra-monkeypox-microdata/blob/master/code.R

library(readr)
library(sdcMicro)
library(dplyr)
library(httr)

url <- "https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv"
data <- readr::read_csv(content(GET(url)),
                        col_types="iccccccDDccDcDicicciccccccccDDc")
data <- rename(data, Hospitalised = "Hospitalised (Y/N/NA)")
selectedKeyVars <- c('Location', 'City', 'Country_ISO3', 'Gender', 'Hospitalised',
                     'Date_hospitalisation', 'Travel_history_location')

# Convert variables into factors
cols =  c('Location', 'City', 'Country_ISO3', 'Gender', 'Hospitalised',
          'Date_hospitalisation', 'Travel_history_location')

data[,cols] <- lapply(data[,cols], factor)

# Convert the sub file into dataframe
fileRes <- as.data.frame(data[,c(selectedKeyVars)])

objSDC <- createSdcObj(dat = fileRes, keyVars = selectedKeyVars)
print(objSDC, "risk")
print(objSDC)
report(objSDC, internal = T)
