library(ggplot2)
library(usmap)
library(ggthemes)
library(tibble)
library(viridis)
library(dplyr)


df = read.csv("vis_dat.csv")

df = df %>% 
  filter(Lon > -150 & Lon < -50 & Lat > 20 & Lat< 50)
df = df[!is.na(df$D2_Hi),]
state <- map_data("state")
county <- map_data("county")
usa <- map_data("usa")

ggplot()+ 
  stat_density2d_filled(data = df, aes(y = Lat,x= Lon, fill = D2_Hi))+ 
  geom_map(data=state, map=state,
                    aes(long, lat, map_id=region),
                    color="white", fill=NA, size=0.2)+
  geom_map(data=usa, map=usa,
                    aes(long, lat, map_id=region),
                    color="white", fill=NA, size=.01)+
  coord_map("polyconic")+
  theme_minimal() +
  labs(colour = "Temp") +
  theme(legend.position="none")





#----------------
ggplot()+ 
  # geom_map(data=county, map=county,
  #                   aes(long, lat, map_id=region),
  #                   color="#2b2b2b", fill=NA, size=0.1)+
  geom_map(data=state, map=state,
           aes(long, lat, map_id=region),
           color="black", fill=NA, size=0.2)+
  geom_map(data=usa, map=usa,
           aes(long, lat, map_id=region),
           color="black", fill=NA, size=.01)+
  coord_map("polyconic")+
  theme_minimal() +
  
  geom_point(data = df2, aes(y = Lat,x= Lon, colour = temp , stroke = 2))+
  scale_color_viridis(option="magma")+ 
  labs(colour = "Temp") +
  theme(legend.position="right")


ggplot(data = df, aes(y = Lat,x= Lon, z = D2_Lo))+ 
  stat_density2d_filled(alpha = .2)





library(mapproj)

data(unemp, package = "viridis")

county_df <- map_data("county", projection = "albers", parameters = c(39, 45))
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

state_df <- map_data("state", projection = "albers", parameters = c(39, 45))

choropleth <- merge(county_df, unemp, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]

ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rate), colour = alpha("white", 1 / 2), size = 0.2) +
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("US unemployment rate by county") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  scale_fill_viridis(option="magma")

ggplot(data = df, aes(x = Lon, y = Lat, z = D2_Hi)) +
  geom_tile(aes(fill = D2_Hi)) +
  stat_contour()+  scale_fill_continuous(name = "Rain (mm)",
                                        low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) 
library(akima)
fld <- with(df, interp(x = Lon, y = Lat, z = D1_Hi))
filled.contour(x = fld$x,
               y = fld$y,
               z = fld$z)


#----------
library(ggplot2)
library(reshape2)
library(akima)
# prepare data in long format
df = read.csv("vis_dat.csv")
df = df[!is.na(df$Lat),]
fld <- with(df, interp(x = Lon, y = Lat, z = D1_Hi))
df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "Rain")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]

ggplot(data = df, aes(x = Lon, y = Lat, z = Rain)) +
  geom_tile(aes(fill = Rain)) +
  stat_contour() +
  ggtitle("Rwandan rainfall") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Rain (mm)",
                        low = "white", high = "blue") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 0.2),
        legend.text = element_text(size = 12))

        