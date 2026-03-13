
# Script to compile and organize locality data for the lizards


install.packages("readxl")
install.packages("tidygeocoder")

library(dplyr)
library(readxl)
library(tidyr)
library(tidygeocoder)
library(readr)

# Step 1. Convert xlsx files to csvs
xl_folder <- "local_input/"
out_folder <- "local_output/"

files <- list.files(xl_folder, pattern = ".xlsx")

# Open each excel file, and write out with different extension
for (i in files){
  readxl::read_excel(paste0(xl_folder, i)) %>%
    write.csv(., paste0(out_folder ,gsub(".xlsx", ".csv", i)))
}

# Step 2. Wrangle/deal with data on each sheet. Check for longitude and latitude

files_formatted <- list.files(out_folder, pattern = ".csv")

# read in all the csvs in the formatted folder
for (i in files_formatted){
  name <- gsub(".csv", "", i)
  assign(name, read.csv(file.path(out_folder, i)), envir = .GlobalEnv)
  }

# Format main datasheet
McNew_WhiptailExtractions_DataSheetInfo <- read.csv("local_output/McNew_WhiptailExtractions_DataSheetInfo.csv") %>%
  mutate(Specimen.Number = gsub("AMNO", "ANMO", Specimen.Number))

# AJB: --------------------------------------------------------------------
# AJB: change field number to Specimen Number, take out space in field number, take out Ns and Es in lat long

AJB <- AJB %>% rename(Specimen.Number = Field.Number)
AJB <- AJB %>% mutate(Specimen.Number = gsub(" ", "", Specimen.Number))
AJB <- AJB %>% mutate(Latitude = gsub("N ", "", Latitude))
AJB <- AJB %>% mutate(Longitude = gsub("E ", "", Longitude))

head(AJB)
# Subset and add to main datasheet
main_ABJ <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "AJB") %>%
  left_join(., select(AJB, Specimen.Number, Latitude, Longitude, Sex, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# AllRAD ------------------------------------------------------------------

AllRAD <- read.csv("local_output/Allrad.csv")
# AllRAD: change X..colector to Specimen Number, take out space, change Lat and Long fields
AllRAD <- AllRAD %>%
          rename(Specimen.Number = X..colector) %>%
          mutate(Specimen.Number = gsub("\\s+", "", Specimen.Number)) %>%
          rename(Latitude = Latitud) %>%
          rename(Longitude = Longitud) %>%
          rename(Sex = Sexo)


main_AllRAD <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "AllRAD") %>%
  left_join(., select(AllRAD, Specimen.Number, Latitude, Longitude, Sex))


# Some of the Allrad info is in Museum Number column. Split the frame into ones
# where the Longitude info was merged and those where it was not.
main_AllRAD1 <- main_AllRAD %>% filter(!is.na(Longitude))
main_AllRAD2 <- main_AllRAD %>% filter(is.na(Longitude))

# Reformat AllRAD frame so include the Musuem Number instead

AllRADb <- AllRAD %>%
  rename(Specimen.Number2 = Specimen.Number) %>%
  rename(Specimen.Number = Museum.Number) %>%
  mutate(Specimen.Number = gsub("\\s+", "", Specimen.Number))

main_AllRAD2 <- left_join(select(main_AllRAD2, -Latitude, -Longitude, - Sex),
                          select(AllRADb, Specimen.Number, Latitude, Longitude, Sex), by = "Specimen.Number")

dim(main_AllRAD1)
# put back together
main_AllRAD <- bind_rows(main_AllRAD1, main_AllRAD2)

# remove other data frames
rm(main_AllRAD1)
rm(main_AllRAD2)
# ANMH --------------------------------------------------------------------
View(AMNH)
# AMNH, rename Lat/Long column and format specimen number column
# lots of data missing here
AMNH <- AMNH %>%
  rename(Latitude = Latitude..Centroid.Dec.,
         Longitude = Longitude..Centroid.Dec.) %>%
  mutate(Specimen.Number = paste0("AMNH", AMCC.Number)) %>%
  rename(Date = Collection.Date..from.)

main_AMNH <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "AMNH") %>%
  left_join(., select(AMNH, Specimen.Number, Latitude, Longitude, Date, Sex)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# ANMO 2021 and 1 ---------------------------------------------------------


# ANMO_2021 and 1: lets combine these sheets. Some of the lat/long info is in
# decimal degrees, some in degrees min seconds.
ANMO <- rbind(ANMO_2021, ANMO1) %>%
 mutate(Specimen.Number = gsub(" ", "", Field.Number))

ANMO$needs_fix <- grepl(" ", ANMO$Latitude)


ANMO_fine <- filter(ANMO, needs_fix == FALSE)
ANMO_fix <- filter(ANMO, needs_fix == TRUE)
nrow(ANMO_fine) + nrow(ANMO_fix) == nrow(ANMO) # split cleanly

ANMO_fix <- ANMO_fix %>%
  rename(Lat_min = Latitude, Long_min = Longitude) %>%
  mutate(Lat_min = gsub("\'", "", Lat_min)) %>%
  mutate(Long_min = gsub("\'", "", Long_min)) %>%
  separate_wider_delim(Lat_min, names = c("lat_deg", "lat_min"), delim = " ", cols_remove = F) %>%
  separate_wider_delim(Long_min, names = c("long_deg", "long_min"), delim = " ", cols_remove = F) %>%
  mutate(Latitude = as.numeric(lat_deg) + as.numeric(lat_min)/60 ) %>%
  mutate(Longitude = as.numeric(long_deg) + as.numeric(long_min)/60 )

# match formatting
ANMO_fine <- ANMO_fine %>% mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

# Put data frames back together, fix typos
ANMO <- bind_rows(ANMO_fix, ANMO_fine) %>%
  mutate(Specimen.Number = gsub("AMNO", "ANMO", Specimen.Number)) %>%
  unique() # some duplicates exist


main_ANMO <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "ANMO_2021" | LocalityInformation == "ANMO1") %>%
  left_join(., select(ANMO, Specimen.Number, Latitude, Longitude, Date, Sex)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# ANMO2 -------------------------------------------------------------------

View(ANMO2)
ANMO2 <- ANMO2 %>%
  mutate(Specimen.Number = paste0(Colector, X..colector)) %>% unique

main_ANMO2 <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "ANMO2") %>%
  left_join(., select(ANMO2, Specimen.Number, Latitude, Longitude, Sex)) %>%
  mutate(Date = NA) %>% unique

# ANMO3 -------------------------------------------------------------------

View(ANMO3)
ANMO3 <- ANMO3 %>%
  mutate(Specimen.Number = paste0(Colector, X..colector)) %>%
  rename(Latitude = Latitud, Longitude = Longitud, Sex = Sexo)

main_ANMO3 <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "ANMO3") %>%
  left_join(., select(ANMO3, Specimen.Number, Latitude, Longitude, Sex)) %>%
  mutate(Date = NA) %>% unique


# ANMO5 -------------------------------------------------------------------
# Check all longitudes should be negative

View(ANMO5)
names(ANMO5)
ANMO5 <- ANMO5 %>%
  mutate(Specimen.Number = gsub(":", "", etiquetaCampo)) %>%
 # rename(Latitude = latitudDecimal, Longitude = longitudDecimal) %>%
  mutate(Longitude = abs(Longitude) * -1) %>%
  #rename(Sex = sexo) %>%
  mutate(mesColecta = case_when(mesColecta == "junio" ~ 6,
                                mesColecta == "Junio" ~ 6,
                                TRUE ~ as.numeric(mesColecta))) %>%
  mutate(Date = paste(añoColecta, mesColecta, díaColecta, sep = "-"))


main_ANMO5 <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "ANMO5") %>%
  left_join(., select(ANMO5, Specimen.Number, Latitude, Longitude, Sex, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# Guatemala ---------------------------------------------------------------

View(Guatemala)
Guatemala <- Guatemala %>%
  mutate(Specimen.Number = gsub(":", "", etiquetaCampo)) %>%
  rename(Latitude = latitudDecimal, Longitude = longitudDecimal, Sex = sexo) %>%
  mutate(Date = paste(añoColecta, "6", díaColecta, sep = "-"))

main_Guatemala <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "Guatemala") %>%
  left_join(., select(Guatemala, Specimen.Number, Latitude, Longitude, Sex, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# JEC ---------------------------------------------------------------------
View(JEC)
JEC <- JEC %>% rename(Specimen.Number = Sample.Number)
main_JEC <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "JEC") %>%
  left_join(., select(JEC, Specimen.Number, Latitude, Longitude, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Sex = NA)

# JEC2: do by hand



# LJL ---------------------------------------------------------------------
# Are the LVLs on our sheet LJLs?
View(LJL)
LJL <- LJL %>%
  rename(Specimen.Number = Record) %>%
  mutate(Specimen.Number = gsub("LJL", "LVL", Specimen.Number)) %>%
  mutate(Date = paste(Year, Month_no, Day, sep = "-")) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
main_LJL <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "LJL") %>%
  left_join(., select(LJL, Specimen.Number, Latitude, Longitude, Date)) %>%
  mutate(Sex = NA)


# RCT ---------------------------------------------------------------------

head(RCT)
RCT <- RCT %>%
  mutate(Specimen.Number = gsub(" ", "", Field.Number)) %>%
  rename(Sex = sex) %>%
  rename(Latitude = latitude) %>%
  rename(Longitude = longitude)

main_RCT <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "RCT") %>%
  left_join(., select(RCT, Specimen.Number, Latitude, Longitude, Sex, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# Reeder ------------------------------------------------------------------

head(Reeder)
Reeder <- Reeder %>% mutate(Specimen.Number = gsub(" ", "", Field.Number))

main_Reeder <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "Reeder") %>%
  left_join(., select(Reeder, Specimen.Number, Latitude, Longitude)) %>%
  mutate(Sex = NA, Date = NA)


# Sullivan ----------------------------------------------------------------
# will need to look at by hand.



# UAEH --------------------------------------------------------------------

UAEH <- UAEH %>%
  mutate(COLLECTOR.NUMBER = sprintf("%03s", COLLECTOR.NUMBER)) %>%
  mutate(Specimen.Number = paste0(COLLECTOR.ACRONYM, COLLECTOR.NUMBER)) %>%
  mutate(num.month = match(COLLECTION.MONTH, month.name)) %>%
  mutate(Date = as.Date(paste(COLLECTION.YEAR, num.month, COLLECTION.DAY, sep = "-"), format = "%Y-%m-%d")) %>%
  rename(Sex = SEX, Latitude = LATITUDE, Longitude = LONGITUDE)


main_UAEH <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "UAEH") %>%
  left_join(., select(UAEH, Specimen.Number, Latitude, Longitude, Sex, Date))

# UTEP --------------------------------------------------------------------

View(UTEP)
UTEP <- UTEP %>%
  mutate(Specimen.Number = gsub(":Herp:", "", GUID)) %>%
  rename(Latitude = DEC_LAT, Longitude = DEC_LONG,
         Sex = SEX, Date = ENDED_DATE)


main_UTEP <- McNew_WhiptailExtractions_DataSheetInfo %>%
  filter(LocalityInformation == "UTEP") %>%
  left_join(., select(UTEP, Specimen.Number, Latitude, Longitude, Sex, Date)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


# Combine info ------------------------------------------------------------
alldfs <- mget(ls(pattern = "^main_"))
str(alldfs)

alldfs <- lapply(alldfs, function(x) {
  mutate(x, Latitude = as.character(Latitude))
})

alldfs <- lapply(alldfs, function(x) {
  mutate(x, Longitude = as.character(Longitude))
})

# manually check that the number of rows lines up with what we're looking for
table(McNew_WhiptailExtractions_DataSheetInfo$LocalityInformation)
sapply(alldfs, nrow)

# bind rows together
combined_data <- bind_rows(alldfs)
dim(combined_data)

McNew_WhiptailExtractions_DataSheetInfo_compiled <-
  left_join(McNew_WhiptailExtractions_DataSheetInfo, combined_data)

dim(McNew_WhiptailExtractions_DataSheetInfo_compiled) # dimensions are right.

# All longitudes should be negative


McNew_WhiptailExtractions_DataSheetInfo_compiled <-
  McNew_WhiptailExtractions_DataSheetInfo_compiled %>%
  mutate(Longitude = abs(as.numeric(Longitude)) * -1)
# Add some locality information based on coordinates and final steps ------------------------------------------

McNew_WhiptailExtractions_DataSheetInfo_compiled_test <-
  McNew_WhiptailExtractions_DataSheetInfo_compiled %>%
  reverse_geocode(
    lat = Latitude,
    long = Longitude,
    method = "osm",
    full_results = TRUE
  )

McNew_WhiptailExtractions_DataSheetInfo_compiled_test <-
McNew_WhiptailExtractions_DataSheetInfo_compiled_test %>%
  select(X, Specimen.Number, Alternative.tube.info, DNA.Concentration..ng.uL.,
         Species, LocalityInformation, Latitude, Longitude, Sex, Date,
         county, state, country)

# clean up sex information

McNew_WhiptailExtractions_DataSheetInfo_compiled_test <-
McNew_WhiptailExtractions_DataSheetInfo_compiled_test %>%
  mutate(Sex = case_when(Sex == "?" ~ NA,
                         Sex == "F" ~ "F",
                         Sex == "female" ~ "F",
                         Sex == "hembra" ~ "F",
                         Sex == "juvenile male" ~ "M",
                         Sex == "Macho" ~ "M",
                         Sex == "male" ~ "M",
                         Sex == "Male" ~ "M"))



# write out info
write_excel_csv(McNew_WhiptailExtractions_DataSheetInfo_compiled_test, "McNew_WhiptailExtractions_DataSheetInfo_compiled.csv")


