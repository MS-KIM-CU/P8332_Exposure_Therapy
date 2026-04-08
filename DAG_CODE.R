# Install required package if not already installed
library(dagitty)
library(ggdag)
library(ggplot2)

# Define the DAG
dag <- dagitty("
dag {
  average_precip -> asthma_er_visits
  
  boroughs -> average_precip
  boroughs -> asthma_er_visits
  
  PM25 -> asthma_er_visits
  
  pollen_count -> asthma_er_visits
}
")

# Optional: coordinates for nicer plotting
coordinates(dag) <- list(
  x = c(average_precip=0, asthma_er_visits=2, boroughs=-1, PM25=0, pollen_count=1),
  y = c(average_precip=1, asthma_er_visits=1, boroughs=2, PM25=2, pollen_count=2)
)

# Plot DAG
plot(dag, main="DAG: Precipitation, Asthma, Borough as Confounder, PM2.5 Effect Modifier, Pollen Mixture")