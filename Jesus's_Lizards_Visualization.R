# Packages ----------------------------------------------------------------
library(curl)
library(readxl)
library(data.table)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(tweenr)
library(ggthemes)
library(viridis)
library(rgeos)
library(countrycode)
library(gganimate)
library(countrycode)

# Data loading and early manipulations ------------------------------------
ISO <- fread("iso.csv")
corona <- fread('train.csv')

corona_summed <- corona %>% 
  group_by(`Country_Region`,Date) %>% 
  summarise(confirmed = sum(ConfirmedCases))

ISO <- ISO %>% 
  select(c(1,3)) %>% 
  rename(`Country_Region` = 1,
         ISO3 = 2)

day1 <- sort(corona_summed$Date)[1] # Data in this file starts at this date
dates <- seq(as.Date(day1), by = "day", length.out = length(unique(corona_summed$Date)))

# Country code fixes --------------------------------------------------------
corona_summed <- corona_summed %>% 
  mutate(ISO3 = countrycode(`Country_Region`, "country.name", "iso3c")) # Setting country codes by country names

existing_countries <- unique(corona_summed$ISO3) # Countries we have in our data

'%!in%' <- function(x,y)!('%in%'(x,y)) # A little function goes a long way

ISO <- ISO %>% 
  add_row(Country_Region = "South Sudan", ISO3 = "SSD") # Trouble maker

nodata_countries <- ISO[ISO$ISO3 %!in% existing_countries,] %>% # Creating a df for countries with no data
  select(1,2) %>% 
  rename(`Country_Region` = 1,
         ISO3 = 2) %>% 
  mutate(Date = as.factor(day1),
         confirmed = 0)

for(i in 1:length(dates)) { # That's one way to do that... Setting missing countries data to 0 for plotting 
  nodata_countries <- nodata_countries %>% 
    mutate(Date = dates[i], confirmed = 0) 
  nodata_countries$Date <- as.factor(nodata_countries$Date) 
  corona_summed <- rbind.data.frame(corona_summed,nodata_countries)
}

corona_summed$Date <- as.Date(corona_summed$Date)

sum(is.na(corona_summed$ISO3)) # Checking NAs status
corona_summed <- corona_summed[!is.na(corona_summed$ISO3),] # Applying a quick NA fix

# Getting country polygons for spatial representation ---------------------
wmap <- getMap(resolution="low")
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <- subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica

centroids <- gCentroid(wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "ISO3")

wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, corona_summed, by = c('id'='ISO3')) # data
wmap_df <- left_join(wmap_df, centroids, by = c('id'='ISO3')) # centroids
wmap_df$log_confirmed <- log10(wmap_df$confirmed) # Log transform the cases scale for better representation

# Plotting and animating --------------------------------------------------
static <- ggplot(data=wmap_df) + 
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill=log_confirmed),
               color="gray80") +
  scale_fill_continuous(name="Confirmed Cases (log)",
                        low = "#F9E79F", high = "#cc2716") +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(caption = "@MaayanKlimenko") +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 0.05,
                                  size=25)) +
  theme(plot.caption = element_text(hjust = 0,
                                    color="gray40",
                                    size=15)) +
  coord_cartesian(xlim = c(-11807982, 14807978)) +
  theme(legend.position = c(.5, .08),
        legend.direction = "horizontal",
        legend.title.align = 0,
        legend.key.size = unit(1.3, "cm"),
        legend.title=element_text(size=17),
        legend.text=element_text(size=13)) +
  transition_manual(Date) +
  labs(title = "World-wide confirmed COVID-19 cases - {current_frame}") 

animap <- animate(static,
                  duration = 25,
                  end_pause = 50,
                  height = 800,
                  width = 1200)

anim_save("covid_anim.gif")