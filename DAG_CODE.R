# Packages
library(dagitty)
library(ggdag)
library(ggplot2)

# Define the DAG
dag <- dagitty("
dag {
  average_precip -> asthma_er_visits
  
  boroughs -> average_precip
  boroughs -> asthma_er_visits
  
  seasons -> average_precip
  seasons -> asthma_er_visits
  
  PM25 -> asthma_er_visits
  
  pollen_count -> asthma_er_visits
}
")

# Optional: coordinates for nicer plotting
coordinates(dag) <- list(
  x = c(average_precip=0, asthma_er_visits=3, boroughs=-1, seasons=-1, PM25=1, pollen_count=2),
  y = c(average_precip=2, asthma_er_visits=2, boroughs=3, seasons=1, PM25=3, pollen_count=1)
)

# Plot DAG
plot(dag, main="DAG: Precipitation, Asthma, Confounders, Effect Modifier, Pollen Mixture")