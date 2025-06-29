rm(list=ls()) # start off by clearing local environment

dropbox_project_sync_off <- function() { # here's a code chunk cribbed from elsewhere to make working on dropbox easier.
  require(usethis)
  this_project <- usethis::proj_get()
  
  if (grep("Dropbox", this_project) == 0) {warning("This project is not in a Dropbox folder")}
  
  dir_to_block <- paste0(this_project,"/.Rproj.user")
  file_to_block <- paste0(this_project,".Rproj")
  
  dir_to_block <- gsub("/", "\\\\", dir_to_block)
  file_to_block <- gsub("/", "\\\\", file_to_block)
  
  # Powershell command examples:
  # These set flags to prevent syncing
  # Set-Content -Path C:\Users\myname\Dropbox\mywork\test\test.Rproj -Stream com.dropbox.ignored -Value 1
  # Set-Content -Path C:\Users\myname\Dropbox\mywork\test\.Rproj.user -Stream com.dropbox.ignored -Value 1
  
  s1 <- paste0('powershell -Command \"& {Set-Content -Path ', file_to_block, ' -Stream com.dropbox.ignored -Value 1}\"')
  s2 <- paste0('powershell -Command \"& {Set-Content -Path ', dir_to_block, ' -Stream com.dropbox.ignored -Value 1}\"')
  
  shell(s1)
  shell(s2)
}

dropbox_project_sync_off() # tell dropbox to ignore all the dropbox system files

library(tidyverse) # for data management
library(magrittr) # for expanded pipes
library(haven) # for foreign data 
library(patchwork) # for plots, don't need here
library(viridis) # earlier versions of this plot used a color scale
library(extrafont) # for prettier fonts
library(sf) # for manipulating spatial data

extrafont::loadfonts(quiet = TRUE) # initialize extra fonts

load("E:\\Dropbox\\My Functions.RData") # load personal functions, particularly exporting plots

library(tidycensus) # also load tidycensus for querying ACS data--you'll need a census API key here: https://api.census.gov/data/key_signup.html

vars_23 <- load_variables(dataset="acs5", year = 2023) # start off by examining potential data fields

df <- get_acs(geography = "tract", # pull data at census tract level
              state = "va", # state is Virginia
              county = c("Roanoke City", # three local communities 
                         "Roanoke County",
                         "Salem City"),
              variables = c("C27007_004", # taken together, these six tables capture recipients of
                            "C27007_007", # means-tested health insurance programs, most likley
                            "C27007_010", # CHIP and Medicaid. They represent a good overview
                            "C27007_014", # of the sorts of programs that have been under threat
                            "C27007_017", # in the last six months. You could, of course, pull
                            "C27007_020"), # others, by exploring `vars_23`
              geometry = T) %>% # get the map
  group_by(GEOID) %>% # within each census tract...
  summarize(sum_est = sum(estimate))  # sum the estimates from the six tables outlined above
  
# producing a chloropleth map highlights that people in more urban areas use these programs,
# which is arguably counterproductive. Instead, let's highlight the total number of people
# who benefit from these policies

sample_points <- st_sample(df, # let's take the dataframe of census tracts
                           size = df$sum_est, # and create a random distribution of dots equal to the ACS estimate
                             type = 'random', exact = TRUE)  %>% # in a random position within the tract shapefile
  st_sf('ID' = seq(length(.)), 'geometry' = .) %>% # let's attach an ID for ease of use later
  st_intersection(., df) # and associate them with a census tract so that we can sample within neighborhoods.

# when I plotted all the sample points it's so cluttered--plotting 45k+ observations across skewed geographies
# is not neat and tidy. So let's sample 10%.

sample_points_10 <- sample_points %>% # start with all the points
  group_by(GEOID) %>% # within each census tract
  slice_sample(prop = .1)  # sample 10%


p <- ggplot() + # time to plot!
  geom_sf(data = df, # first, census tract boundaries
          aes(geometry = geometry), size = 1) +
  geom_sf(data = sample_points_10, # then our sample dataset.
             aes(geometry = geometry),
          color = "black") +
  theme_void() + # people don't need to see lat and lon
  ggtitle("Each dot represents 10 of our 45,463\nneighbors who rely on Medicaid and CHIP.") + # a title
  theme(plot.title = element_text(family = "Roboto", size = 18, hjust = .5)) # slightly nicer font

mygg(p, "chip medicaid", 6,5) # save everything!


