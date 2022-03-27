---
title: 'Weekly Exercises #4'
author: "Kate Nozal"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---





```r
library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(carData)       # for Minneapolis police stops data
library(ggthemes)      # for more themes (including theme_map())
theme_set(theme_minimal())
```


```r
# Starbucks locations
Starbucks <- read_csv("https://www.macalester.edu/~ajohns24/Data/Starbucks.csv")

starbucks_us_by_state <- Starbucks %>% 
  filter(Country == "US") %>% 
  count(`State/Province`) %>% 
  mutate(state_name = str_to_lower(abbr2state(`State/Province`))) 

# Lisa's favorite St. Paul places - example for you to create your own data
favorite_stp_by_lisa <- tibble(
  place = c("Home", "Macalester College", "Adams Spanish Immersion", 
            "Spirit Gymnastics", "Bama & Bapa", "Now Bikes",
            "Dance Spectrum", "Pizza Luce", "Brunson's"),
  long = c(-93.1405743, -93.1712321, -93.1451796, 
           -93.1650563, -93.1542883, -93.1696608, 
           -93.1393172, -93.1524256, -93.0753863),
  lat = c(44.950576, 44.9378965, 44.9237914,
          44.9654609, 44.9295072, 44.9436813, 
          44.9399922, 44.9468848, 44.9700727)
  )

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```


## Warm-up exercises from tutorial

These exercises will reiterate what you learned in the "Mapping data with R" tutorial. If you haven't gone through the tutorial yet, you should do that first.

### Starbucks locations (`ggmap`)

  1. Add the `Starbucks` locations to a world map. Add an aesthetic to the world map that sets the color of the points according to the ownership type. What, if anything, can you deduce from this visualization?  

```r
world <- get_stamenmap(
    bbox = c(left = -180, bottom = -57, right = 179, top = 82.1), 
    maptype = "terrain",
    zoom = 2)

ggmap(world) + 
  geom_point(data = Starbucks, 
             aes(x = Longitude, y = Latitude, color = `Ownership Type`)) +
  theme_map()+
  labs(title= "Types of Starbucks Across the World")
```

![](04_exercises_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
 Joint ventures are only in Asia and Eastern Europe. There isnt much to tell in the Western hemisphere but there seems to be some trends in Asia that are likely drawn along borders or changes in economic policy. 
 
  2. Construct a new map of Starbucks locations in the Twin Cities metro area (approximately the 5 county metro area).  

```r
twin_cities <- get_stamenmap(
  bbox = c(left= -93.7543, bottom = 44.6144, right = -92.4575, top = 45.3275),
  maptype = "terrain", 
  zoom = 10)

ggmap(twin_cities) +
  geom_point(data = Starbucks,
             aes(x= Longitude, y= Latitude), color = "red") +
  theme_map() +
  labs(title= "Starbucks Locations in the Twin Cities")
```

![](04_exercises_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

  3. In the Twin Cities plot, play with the zoom number. What does it do?  (just describe what it does - don't actually include more than one map).  

```r
twin_cities <- get_stamenmap(
  bbox = c(left= -93.7543, bottom = 44.6144, right = -92.4575, top = 45.3275),
  maptype = "terrain", 
  zoom = 12)

ggmap(twin_cities) +
  geom_point(data = Starbucks,
             aes(x= Longitude, y= Latitude), color = "red") +
  theme_map()+
  labs(title= "Starbucks in the Twin Cities")
```

![](04_exercises_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
Zoom changes the detail of the map. 10 is the typical zoom but lower would allow you to see more and a higher zoom would show less but more detail.   
  4. Try a couple different map types (see `get_stamenmap()` in help and look at `maptype`). Include a map with one of the other map types. 

```r
twin_cities <- get_stamenmap(
  bbox = c(left= -93.7543, bottom = 44.6144, right = -92.4575, top = 45.3275),
  maptype = "toner-hybrid", 
  zoom = 10)

ggmap(twin_cities) +
  geom_point(data = favorite_stp_by_lisa,
             aes(x= long, y= lat), color = "red") +
  theme_map()+
  labs(title= "Lisa's Faovrite Places")
```

![](04_exercises_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

  5. Add a point to the map that indicates Macalester College and label it appropriately. There are many ways you can do think, but I think it's easiest with the `annotate()` function (see `ggplot2` cheatsheet).

```r
twin_cities <- get_stamenmap(
  bbox = c(left= -93.2339, bottom = 44.8940, right = -93.0468, top = 44.9832),
  maptype = "terrain", 
  zoom = 12)

ggmap(twin_cities) +
  geom_point(data = Starbucks,
             aes(x= Longitude, y= Latitude), color = "red") +
  theme_map() +
  annotate("point", x= -93.1691, y = 44.933, label = "Macalester College", color = "blue") +
  annotate("text", x= -93.1691, y = 44.931, label = "Macalester College")+
  labs(title= "Starbucks Locations Around Macalester College")
```

![](04_exercises_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Choropleth maps with Starbucks data (`geom_map()`)

The example I showed in the tutorial did not account for population of each state in the map. In the code below, a new variable is created, `starbucks_per_10000`, that gives the number of Starbucks per 10,000 people. It is in the `starbucks_with_2018_pop_est` dataset.


```r
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

starbucks_with_2018_pop_est <-
  starbucks_us_by_state %>% 
  left_join(census_pop_est_2018,
            by = c("state_name" = "state")) %>% 
  mutate(starbucks_per_10000 = (n/est_pop_2018)*10000)
```

  6. **`dplyr` review**: Look through the code above and describe what each line of code does.
1- saving the new data set
2-
3-

  7. Create a choropleth map that shows the number of Starbucks per 10,000 people on a map of the US. Use a new fill color, add points for all Starbucks in the US (except Hawaii and Alaska), add an informative title for the plot, and include a caption that says who created the plot (you!). Make a conclusion about what you observe.

```r
starbucks_us_by_state <- Starbucks %>% 
  filter(Country == "US") %>% 
  count(`State/Province`) %>% 
  mutate(state_name = str_to_lower(abbr2state(`State/Province`))) 

usa <- map_data("state")
starbucks_with_2018_pop_est %>% 
  ggplot()+
  geom_map(map= usa, 
           aes(map_id= state_name, fill = starbucks_per_10000)) +
  geom_point(data= Starbucks %>% filter(Country == "US", `State/Province` != "AK"), aes(x=Longitude, y=Latitude), color = "red", size = 0.2)+
  expand_limits(x = usa$long, y = usa$lat) + 
  theme_map()+
  labs(title= "Starbucks Locations per 10000 People in 2018", caption= "Created by Kate Nozal")
```

![](04_exercises_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### A few of your favorite things (`leaflet`)

  8. In this exercise, you are going to create a single map of some of your favorite places! The end result will be one map that satisfies the criteria below. 

  * Create a data set using the `tibble()` function that has 10-15 rows of your favorite places. The columns will be the name of the location, the latitude, the longitude, and a column that indicates if it is in your top 3 favorite locations or not. For an example of how to use `tibble()`, look at the `favorite_stp_by_lisa` I created in the data R code chunk at the beginning.  

  * Create a `leaflet` map that uses circles to indicate your favorite places. Label them with the name of the place. Choose the base map you like best. Color your 3 favorite places differently than the ones that are not in your top 3 (HINT: `colorFactor()`). Add a legend that explains what the colors mean.  
  
  * Connect all your locations together with a line in a meaningful way (you may need to order them differently in the original data).  
  
  * If there are other variables you want to add that could enhance your plot, do that now.  
  

```r
favorite_stp_by_Kate <- tibble(
  place = c("Home", "Macalester College", "Highland Park High School", "Boca Chica", "Nelsons", "Caribou", "Lake of the Isles", "Keg and Case Market", "Boom Island Park", "Midtown Global Market"),
  long = c(-93.110138, -93.1712321, -93.167388, -93.083619, -93.166869, -93.166643, -93.307377,-93.124540, -93.269094, -93.260970),
  lat = c(44.763440, 44.9378965, 44.909533,44.933985, 44.927715, 44.939469, 44.954895,44.928872, 44.991906, 44.948460),
  faveplace = c("True", "True", "False", "False", "False","True", "False", "False", "False", "False")
  )

pal <- colorFactor(palette = "magma", domain = favorite_stp_by_Kate$faveplace)

favorite_stp_by_Kate %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerHybrid) %>%
  addCircleMarkers(lng=~long, lat=~lat, color = ~pal(faveplace),  label = ~place) %>% 
   addLegend("bottomright", pal = pal, values = ~faveplace,
    title = "Favorite Place",
    opacity = 1
   ) %>% 
    addPolylines(lng = ~long, 
               lat = ~lat, 
               color = col2hex("darkred"))
```

```{=html}
<div id="htmlwidget-fd901727e630a22d5164" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-fd901727e630a22d5164">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Stamen.TonerHybrid",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[44.76344,44.9378965,44.909533,44.933985,44.927715,44.939469,44.954895,44.928872,44.991906,44.94846],[-93.110138,-93.1712321,-93.167388,-93.083619,-93.166869,-93.166643,-93.307377,-93.12454,-93.269094,-93.26097],10,null,null,{"interactive":true,"className":"","stroke":true,"color":["#FCFDBF","#FCFDBF","#000004","#000004","#000004","#FCFDBF","#000004","#000004","#000004","#000004"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["#FCFDBF","#FCFDBF","#000004","#000004","#000004","#FCFDBF","#000004","#000004","#000004","#000004"],"fillOpacity":0.2},null,null,null,null,["Home","Macalester College","Highland Park High School","Boca Chica","Nelsons","Caribou","Lake of the Isles","Keg and Case Market","Boom Island Park","Midtown Global Market"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#000004","#FCFDBF"],"labels":["False","True"],"na_color":null,"na_label":"NA","opacity":1,"position":"bottomright","type":"factor","title":"Favorite Place","extra":null,"layerId":null,"className":"info legend","group":null}]},{"method":"addPolylines","args":[[[[{"lng":[-93.110138,-93.1712321,-93.167388,-93.083619,-93.166869,-93.166643,-93.307377,-93.12454,-93.269094,-93.26097],"lat":[44.76344,44.9378965,44.909533,44.933985,44.927715,44.939469,44.954895,44.928872,44.991906,44.94846]}]]],null,null,{"interactive":true,"className":"","stroke":true,"color":"#8B0000","weight":5,"opacity":0.5,"fill":false,"fillColor":"#8B0000","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[44.76344,44.991906],"lng":[-93.307377,-93.083619]}},"evals":[],"jsHooks":[]}</script>
```
## Revisiting old datasets

This section will revisit some datasets we have used previously and bring in a mapping component. 

### Bicycle-Use Patterns

The data come from Washington, DC and cover the last quarter of 2014.

Two data tables are available:

- `Trips` contains records of individual rentals
- `Stations` gives the locations of the bike rental stations

Here is the code to read in the data. We do this a little differently than usualy, which is why it is included here rather than at the top of this file. To avoid repeatedly re-reading the files, start the data import chunk with `{r cache = TRUE}` rather than the usual `{r}`. This code reads in the large dataset right away.


```r
data_site <- 
  "https://www.macalester.edu/~dshuman1/data/112/2014-Q4-Trips-History-Data.rds" 
Trips <- readRDS(gzcon(url(data_site)))
Stations<-read_csv("http://www.macalester.edu/~dshuman1/data/112/DC-Stations.csv")
```

  9. Use the latitude and longitude variables in `Stations` to make a visualization of the total number of departures from each station in the `Trips` data. Use either color or size to show the variation in number of departures. This time, plot the points on top of a map. Use any of the mapping tools you'd like.
  

```r
dc_area <- get_stamenmap(
  bbox = c(left= -77.3433, bottom = 38.7517, right = -76.6306, top = 39.0738),
  maptype = "terrain", 
  zoom = 10)

joined_stations <- Stations %>% 
  left_join(Trips,
            by= c("name" = "sstation")) %>% 
  group_by(name) %>% 
  summarize(trip_count =n(), lat, long) 

ggmap(dc_area) +
  geom_point(data=joined_stations, aes(x=long, y=lat, color = trip_count))+
  labs(title= "Total Number of Departures From all Stations")
```

![](04_exercises_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
  10. Only 14.4% of the trips in our data are carried out by casual users. Create a plot that shows which area(s) have stations with a much higher percentage of departures by casual users. What patterns do you notice? Also plot this on top of a map. I think it will be more clear what the patterns are.
  

```r
dc_area <- get_stamenmap(
  bbox = c(left= -77.3433, bottom = 38.7517, right = -76.6306, top = 39.0738),
  maptype = "terrain", 
  zoom = 10)

Trips2<- Trips %>% 
  left_join(Stations,
            by= c("sstation" = "name")) %>% 
 group_by(client, lat, long) %>% 
  summarize(sum_name = n(), client_prop = mean(client == "Casual"))  
 
   ggmap(dc_area) +
  geom_point(data= Trips2, aes(x = long, y = lat, color = client_prop))+
     labs(title= "Trip Departures Comparison for Causal Riders")
```

![](04_exercises_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
### COVID-19 data

The following exercises will use the COVID-19 data from the NYT.

  11. Create a map that colors the states by the most recent cumulative number of COVID-19 cases (remember, these data report cumulative numbers so you don't need to compute that). Describe what you see. What is the problem with this map?

```r
usa <- map_data("state")

covid19 %>% 
  group_by(state) %>% 
  summarize(recent_case = max(cases)) %>% 
  mutate(state = str_to_lower(state)) %>% 
  ggplot(aes(fill= recent_case))+
  geom_map(map= usa, 
           aes(map_id= state))+
  expand_limits(x = usa$long, y = usa$lat) + 
  theme_map()+
  labs(title= "Cumulative Covid Cases by State")
```

![](04_exercises_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
  The map doesn't take into account population so its no surprise that California the most populous state has the most cases. 
  
  12. Now add the population of each state to the dataset and color the states by most recent cumulative cases/10,000 people. See the code for doing this with the Starbucks data. You will need to make some modifications. 

```r
usa <- map_data("state")

covid19_with_2018_pop <-
  covid19 %>% 
  group_by(state) %>% 
  summarize(recent_case = max(cases)) %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018,
            by = "state") %>% 
  mutate(covid19_per_10000 = (recent_case/est_pop_2018)*10000)


covid19_with_2018_pop %>% 
  ggplot()+
  geom_map(map= usa, 
           aes(map_id= state, fill = covid19_per_10000))+
  expand_limits(x = usa$long, y = usa$lat) + 
  theme_map()+
  labs(title= "Cumulative Covid Cases by State per 10000 people")
```

![](04_exercises_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
  
  
## Minneapolis police stops

These exercises use the datasets `MplsStops` and `MplsDemo` from the `carData` library. Search for them in Help to find out more information.

  14. Use the `MplsStops` dataset to find out how many stops there were for each neighborhood and the proportion of stops that were for a suspicious vehicle or person. Sort the results from most to least number of stops. Save this as a dataset called `mpls_suspicious` and display the table.  

```r
mpls_suspicious <-MplsStops %>%
  filter(problem == "suspicious") %>% 
  group_by(neighborhood) %>% 
  summarize(stop_count =n()) %>% 
  arrange(desc(stop_count))

mpls_suspicious
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["neighborhood"],"name":[1],"type":["fct"],"align":["left"]},{"label":["stop_count"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"Downtown West","2":"3338"},{"1":"Whittier","2":"1351"},{"1":"East Phillips","2":"959"},{"1":"Near - North","2":"937"},{"1":"Lyndale","2":"872"},{"1":"Jordan","2":"813"},{"1":"Hawthorne","2":"785"},{"1":"Midtown Phillips","2":"767"},{"1":"Ventura Village","2":"766"},{"1":"Powderhorn Park","2":"746"},{"1":"Lowry Hill East","2":"603"},{"1":"Cedar Riverside","2":"563"},{"1":"Loring Park","2":"548"},{"1":"Willard - Hay","2":"532"},{"1":"Central","2":"528"},{"1":"Marcy Holmes","2":"517"},{"1":"Folwell","2":"485"},{"1":"Phillips West","2":"466"},{"1":"Steven's Square - Loring Heights","2":"447"},{"1":"Longfellow","2":"441"},{"1":"Elliot Park","2":"424"},{"1":"Webber - Camden","2":"385"},{"1":"Seward","2":"380"},{"1":"North Loop","2":"333"},{"1":"Holland","2":"319"},{"1":"McKinley","2":"315"},{"1":"Lind - Bohanon","2":"272"},{"1":"Victory","2":"265"},{"1":"King Field","2":"249"},{"1":"Harrison","2":"245"},{"1":"CARAG","2":"234"},{"1":"Corcoran","2":"220"},{"1":"Prospect Park - East River Road","2":"210"},{"1":"Audubon Park","2":"206"},{"1":"Cleveland","2":"206"},{"1":"ECCO","2":"188"},{"1":"Windom","2":"183"},{"1":"Standish","2":"180"},{"1":"Northrop","2":"172"},{"1":"Nicollet Island - East Bank","2":"163"},{"1":"Hiawatha","2":"161"},{"1":"Windom Park","2":"158"},{"1":"Howe","2":"146"},{"1":"Waite Park","2":"144"},{"1":"Downtown East","2":"143"},{"1":"Como","2":"138"},{"1":"Northeast Park","2":"130"},{"1":"St. Anthony West","2":"128"},{"1":"Linden Hills","2":"124"},{"1":"Lowry Hill","2":"117"},{"1":"Diamond Lake","2":"115"},{"1":"Bancroft","2":"113"},{"1":"East Isles","2":"112"},{"1":"Regina","2":"108"},{"1":"Shingle Creek","2":"106"},{"1":"Ericsson","2":"105"},{"1":"Keewaydin","2":"105"},{"1":"Sheridan","2":"101"},{"1":"Wenonah","2":"101"},{"1":"Tangletown","2":"98"},{"1":"Bottineau","2":"96"},{"1":"Minnehaha","2":"95"},{"1":"Marshall Terrace","2":"93"},{"1":"Fulton","2":"90"},{"1":"Lynnhurst","2":"86"},{"1":"University of Minnesota","2":"84"},{"1":"East Harriet","2":"80"},{"1":"Logan Park","2":"79"},{"1":"Bryn - Mawr","2":"78"},{"1":"Bryant","2":"77"},{"1":"Mid - City Industrial","2":"77"},{"1":"Morris Park","2":"71"},{"1":"Sumner - Glenwood","2":"70"},{"1":"Field","2":"68"},{"1":"St. Anthony East","2":"66"},{"1":"Armatage","2":"65"},{"1":"Kenny","2":"65"},{"1":"Columbia Park","2":"64"},{"1":"Cooper","2":"56"},{"1":"Cedar - Isles - Dean","2":"54"},{"1":"Beltrami","2":"53"},{"1":"Hale","2":"48"},{"1":"Kenwood","2":"48"},{"1":"West Calhoun","2":"45"},{"1":"Page","2":"32"},{"1":"Camden Industrial","2":"12"},{"1":"Humboldt Industrial Area","2":"4"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
  
  15. Use a `leaflet` map and the `MplsStops` dataset to display each of the stops on a map as a small point. Color the points differently depending on whether they were for suspicious vehicle/person or a traffic stop (the `problem` variable). HINTS: use `addCircleMarkers`, set `stroke = FAlSE`, use `colorFactor()` to create a palette. 

```r
pal <- colorFactor(palette = "magma", domain = MplsStops$problem)

MplsStops %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerHybrid) %>%
  addCircleMarkers(lng=~long, lat=~lat, fillColor = ~pal(problem), stroke=FALSE, radius = 2) %>% 
  
```

```
## Error: <text>:9:0: unexpected end of input
## 7:   
## 8: 
##   ^
```
  
  16. Save the folder from moodle called Minneapolis_Neighborhoods into your project/repository folder for this assignment. Make sure the folder is called Minneapolis_Neighborhoods. Use the code below to read in the data and make sure to **delete the `eval=FALSE`**. Although it looks like it only links to the .sph file, you need the entire folder of files to create the `mpls_nbhd` data set. These data contain information about the geometries of the Minneapolis neighborhoods. Using the `mpls_nbhd` dataset as the base file, join the `mpls_suspicious` and `MplsDemo` datasets to it by neighborhood (careful, they are named different things in the different files). Call this new dataset `mpls_all`.


```r
mpls_nbhd <- st_read("Minneapolis_Neighborhoods/Minneapolis_Neighborhoods.shp", quiet = TRUE)
```

```
## Error: Cannot open "Minneapolis_Neighborhoods/Minneapolis_Neighborhoods.shp"; The file doesn't seem to exist.
```

```r
mpls_all <- mpls_nbhd %>% 
  left_join(mpls_suspicious,
            by = c( "BDNAME" = "neighborhood")) %>% 
  left_join(MplsDemo,
            by = c("BDNAME" = "neighborhood"))
```

```
## Error in left_join(., mpls_suspicious, by = c(BDNAME = "neighborhood")): object 'mpls_nbhd' not found
```

  17. Use `leaflet` to create a map from the `mpls_all` data  that colors the neighborhoods by `prop_suspicious`. Display the neighborhood name as you scroll over it. Describe what you observe in the map.

```r
pal <- colorFactor(palette = "magma", domain = MplsStops$prop_suspicious)

 
mpls_all %>%
  mutate(prop_suspicious = stop_count/population) %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerHybrid) %>%
  addPolygons(fillColor = ~pal(prop_suspicious), label = ~BDNAME) %>% 

  
```

```
## Error: <text>:12:0: unexpected end of input
## 10:   
## 11: 
##    ^
```
  
  
  18. Use `leaflet` to create a map of your own choosing. Come up with a question you want to try to answer and use the map to help answer that question. Describe what your map shows. 

```r
pal <- colorFactor(palette = "magma", domain = MplsStops$white)
 
 
mpls_large <- mpls_all %>%
  left_join(MplsStops,
            by = c("BDNAME" = "neighborhood")) %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerHybrid) %>%
  addPolygons(fillColor = ~pal(white), label = ~BDNAME)
```

```
## Error in left_join(., MplsStops, by = c(BDNAME = "neighborhood")): object 'mpls_all' not found
```

```r
  addCircleMarkers(lng=~long, lat=~lat, stroke=FALSE, radius = 2)
```

```
## Error in getMapData(map): argument "map" is missing, with no default
```
  
  
## GitHub link

  19. Below, provide a link to your GitHub page with this set of Weekly Exercises. Specifically, if the name of the file is 04_exercises.Rmd, provide a link to the 04_exercises.md file, which is the one that will be most readable on GitHub.
  
  https://github.com/knozal/comp_112/blob/main/exercises/04_exercises.Rmd


