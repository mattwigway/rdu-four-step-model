# bootstrap the R environment on Google Colab
devtools::install_github("mattwigway/BabysFirstFourStepModel", upgrade="never")

# load libraries
library(bf4sm)
library(tidyverse)
library(gtsummary)
library(sf)

# don't convert the 100k+ income category to 1e5+
options(scipen=10)

rdu_tracts = tigris::tracts(state="NC", county=c("Durham", "Orange", "Wake"))

map_trip_generation = function (trip_counts, timeperiod, triptype) {
    counts = trip_counts %>%
        filter(time_period == timeperiod & trip_type == triptype)
    
    rdu_tracts %>%
        st_transform(32119) %>%
        left_join(counts, by=c("GEOID"="geoid")) %>%
        ggplot(aes(fill=n_trips / ALAND * (1000 * 1000))) +
            geom_sf() +
            scale_fill_fermenter(palette="Blues", direction=1) +
            labs(fill="Daily trips generated\nper square kilometer") +
            ggtitle(paste(triptype, "trips,", timeperiod)) +
            geom_sf(data=model$network_geo, fill="black")
}

map_trip_distribution = function (flows, timeperiod, triptype, origin_tract) {
    from_tract = flows %>%
        filter(time_period == timeperiod & trip_type == triptype & orig_geoid == origin_tract)

    rdu_tracts %>%
        st_transform(32119) %>%
        left_join(from_tract, by=c("GEOID"="dest_geoid")) %>%
        ggplot(aes(fill=n_trips)) +
            geom_sf() +
            scale_fill_fermenter(palette="Greens", direction=1) +
            labs(fill="Number of trips destined for tract") +
            ggtitle(paste(triptype, "trips,", timeperiod, "from tract", origin_tract)) +
            geom_sf(data=filter(rdu_tracts, GEOID==origin_tract), fill="blue") +
            geom_sf(data=model$network_geo, fill="black")

}