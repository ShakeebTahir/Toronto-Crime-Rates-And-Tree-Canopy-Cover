library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(spdep)

#Author: Shakeeb Tahir (1004359553)

# Load In Neighbourhood CSV data
neighbourhood_data <- read_csv("Neighbourhood_Data.csv")

# Create regression models and check if they are significant (P<0.05)
assault_model <- lm(`Assault Rate` ~ `Trees Per Square Km`, data = neighbourhood_data)
summary(assault_model)

auto_theft_model <- lm(`Auto Theft Rate` ~ `Trees Per Square Km`, data = neighbourhood_data)
summary(auto_theft_model)

break_enter_model <- lm(`Break And Enter Rate` ~ `Trees Per Square Km`, data = neighbourhood_data)
summary(break_enter_model)

robbery_model <- lm(`Robbery Rate` ~ `Trees Per Square Km`, data = neighbourhood_data)
summary(robbery_model)


# Plot Tree and Crime Rates
ggplot(data = neighbourhood_data, mapping = aes(x = `Trees Per Square Km`, 
                                                y = `Assault Rate`))+ 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + ggtitle("Trees Per Sq Km vs Assault Rate") +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))


ggplot(data = neighbourhood_data, mapping = aes(x = `Trees Per Square Km`, 
                                                y = `Auto Theft Rate`))+ 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + ggtitle("Trees Per Sq Km vs Auto Theft Rate") +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))


ggplot(data = neighbourhood_data, mapping = aes(x = `Trees Per Square Km`, 
                                                y = `Break And Enter Rate`))+ 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + ggtitle("Trees Per Sq Km vs Break and Enter Rate") +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))


ggplot(data = neighbourhood_data, mapping = aes(x = `Trees Per Square Km`, 
                                                y = `Robbery Rate`))+ 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + ggtitle("Trees Per Sq Km vs Robbery Rate") + 
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))

# Load in shapefile layer with data
neighbourhoods_spdf <- readOGR("C:/Users/Shakeeb/Desktop/Courses/GGR463/Project/Rates_w_Trees", 
                          layer = "FinalLayer")

# Create a list of polygons that share borders
Toronto_nb <- poly2nb(neighbourhoods_spdf)

# Plots neighbour connections
par(mai=c(0,0,0,0))
plot(neighbourhoods_spdf)
plot(Toronto_nb, coordinates(neighbourhoods_spdf), col='red', lwd=2, add=TRUE)

# Convert Neigbours List to Spatial Weights
Toronto_listw <- nb2listw(Toronto_nb)

# Run Moran's I tests
moran.test(neighbourhoods_spdf$Assault_Ra, Toronto_listw)
moran.test(neighbourhoods_spdf$AutoTheft_, Toronto_listw)
moran.test(neighbourhoods_spdf$BreakAndEn, Toronto_listw)
moran.test(neighbourhoods_spdf$Robbery_Ra, Toronto_listw)


