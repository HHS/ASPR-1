# Libraries ---------------------------------------------------------------

library(yaml)
library(tidyverse)

scenarios <- lst()
propertyGroups <- lst()

# TODO - incorporate 'forcedExperimentColumnProperties: POPULATION_DESCRIPTION'

# User specifications -----------------------------------------------------

# Experiment output sub-directory - directory string to be appended to outputDirectory (as defined in localOptions.yaml)
outputSubDir <- "yamlTest/"
yamlFileName <- str_c("test-generated-", lubridate::today(), ".yaml")

# Which of the 5 planning scenarios to run
planningScenariosToRun = c(1, 2, 3, 4, 5)

# Population description
### TODO - generalize this
popName <- c("population/cbsa_all_work_school_household_2020-04-24/cbsa_all_work_school_household/Multi-state/New York-Newark-Jersey City NY-NJ-PA.csv",
             "population/de.csv")
initI <- c(0.0005, 0.01)

scenarios$POPULATION_DESCRIPTION = as.list(popName)
scenarios$INITIAL_INFECTIONS = lapply(initI, 
                                      function(initI) lst(type = "FRACTION", defaultValue = initI))
propertyGroups <- c(
  propertyGroups,
  list(
    lst(name = "Location", properties = c("POPULATION_DESCRIPTION", "INITIAL_INFECTIONS"))
  )
)


# Model seasonality if seasonality == true. If seasonality == true and alsoModelNoSeasonality == true, model
# both options. If seasonality == false, alsoModelNoSeasonality is ignored.
seasonality <- TRUE
alsoModelNoSeasonality <- TRUE

# Mitigation options should be defined as a list of tibbles. Each element of the list
# gives a set of parameters to covary. For each tibble, the first column gives the name of 
# each scenario, and subsequent columns giving the trigger names corresponding to each parameter.
# All mitigation columns will be co-varied.
mitigationDefinition = lst(
  
  # Standard mitigation scenarios
  `Mitigation Parameters` = tibble(
    NAME = c("Moderate Mitigation", "Unmitigated"),
    ISOLATION_HYGIENE_START = c("Day 22 - SS Start", ""), 
    ISOLATION_HYGIENE_END = c("", ""),
    TELEWORK_START = c("Day 22 - SS Start", ""),
    TELEWORK_END = c("", ""),
    LOCATION_INFECTION_REDUCTION_START = c("Day 22 - SS Start", ""), 
    LOCATION_INFECTION_REDUCTION_END = c("", ""),
    SHELTER_IN_PLACE_START = c("Day 32 - SIP Start", ""), 
    SHELTER_IN_PLACE_END = c("Day 62 - SIP End", ""),
    
    SCHOOL_CLOSURE_START = c("Day 22 - SS Start", "Day 22 - SS Start"),
    SCHOOL_CLOSURE_END = c("Day 90 - Summer Start", "Day 90 - Summer Start")
  ),
  
  # Infection targeting ratio
  `Infection targeting ratio` = tibble(
    NAME = c("Random", "Low", "Mid", "High"),
    INFECTION_TARGETING_RATIO = c(1, 3, 10, 30)
  ),
  
  # Random testing frequency
  `Random testing frequency` = tibble(
    NAME = c(0.001, 0.005, 0.01),
    FRACTION_OF_POPULATION_TESTED_DAILY = c(0.001, 0.005, 0.01)
  )
)
  
print(mitigationDefinition)

# Setup -------------------------------------------------------------------

plugin_options <- read_yaml("input/pluginOptions.yaml")

# Scenarios ---------------------------------------------------------------

scenarioParameters <- read_yaml("scenarioParameters.yaml")
scenarios <- c(
  scenarios,
  lst(
    # Parameters that vary across planning scenarios
    AVERAGE_TRANSMISSION_RATIO = scenarioParameters$AVERAGE_TRANSMISSION_RATIO[planningScenariosToRun],
    FRACTION_SYMPTOMATIC = scenarioParameters$FRACTION_SYMPTOMATIC[planningScenariosToRun],
    ASYMPTOMATIC_INFECTIOUSNESS = scenarioParameters$ASYMPTOMATIC_INFECTIOUSNESS[planningScenariosToRun],
    CASE_HOSPITALIZATION_RATIO = scenarioParameters$CASE_HOSPITALIZATION_RATIO[planningScenariosToRun],
    CASE_FATALITY_RATIO = scenarioParameters$CASE_FATALITY_RATIO[planningScenariosToRun],
    SYMPTOMATIC_INFECTIOUS_PERIOD = scenarioParameters$SYMPTOMATIC_INFECTIOUS_PERIOD[planningScenariosToRun],
    SYMPTOMATIC_INFECTIOUS_PERIOD_COV = scenarioParameters$SYMPTOMATIC_INFECTIOUS_PERIOD_COV[planningScenariosToRun],
    FRACTION_LATENT_PERIOD_INFECTIOUS = scenarioParameters$FRACTION_LATENT_PERIOD_INFECTIOUS[planningScenariosToRun],
    
    # Parameters that are constant across planning scenarios
    LATENT_PERIOD = scenarioParameters$LATENT_PERIOD,
    LATENT_PERIOD_COV = scenarioParameters$LATENT_PERIOD_COV,
    FRACTION_OF_GLOBAL_CONTACTS_IN_HOME_REGION = scenarioParameters$FRACTION_OF_GLOBAL_CONTACTS_IN_HOME_REGION,
    TRANSMISSION_OVERDISPERSION = scenarioParameters$TRANSMISSION_OVERDISPERSION,
    HOSPITALIZATION_TO_DEATH_DELAY_MEAN = scenarioParameters$HOSPITALIZATION_TO_DEATH_DELAY_MEAN,
    HOSPITALIZATION_TO_DEATH_DELAY_SD = scenarioParameters$HOSPITALIZATION_TO_DEATH_DELAY_SD,
    HOSPITALIZATION_DELAY_MEAN = scenarioParameters$HOSPITALIZATION_DELAY_MEAN,
    HOSPITALIZATION_DELAY_SD = scenarioParameters$HOSPITALIZATION_DELAY_SD,
    HOSPITALIZATION_DURATION_MEAN = scenarioParameters$HOSPITALIZATION_DURATION_MEAN,
    HOSPITALIZATION_DURATION_SD = scenarioParameters$HOSPITALIZATION_DURATION_SD
  )
)

propertyGroups <- c(
  propertyGroups,
  list(lst(
    name = "Scenario Parameters",
    properties = c(
      "AVERAGE_TRANSMISSION_RATIO",
      "ASYMPTOMATIC_INFECTIOUSNESS",
      "FRACTION_SYMPTOMATIC",
      "SYMPTOMATIC_INFECTIOUS_PERIOD",
      "SYMPTOMATIC_INFECTIOUS_PERIOD_COV",
      "FRACTION_LATENT_PERIOD_INFECTIOUS",
      "CASE_HOSPITALIZATION_RATIO",
      "CASE_FATALITY_RATIO"
    ),
    labels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5")
  ))
)


# Seasonality -------------------------------------------------------------

if (seasonality) {
  seasonalityOptions <- read_yaml("input/seasonalityOptions.yaml")
  
  plugin_options$transmissionPlugin = "SEASONAL"
  scenarios$TRANSMISSION_DECLINE_START = seasonalityOptions$seasonalityDeclineStart
  scenarios$TRANSMISSION_DECLINE_DURATION = seasonalityOptions$seasonalityDeclineDuration
  scenarios$TRANSMISSION_NADIR_DURATION = seasonalityOptions$transmissionNadirDuration
  scenarios$TRANSMISSION_INCLINE_DURATION = seasonalityOptions$transmissionInclineDuration
  scenarios$RELATIVE_TRANSMISSIBILITY_AT_NADIR = list(seasonalityOptions$relativeSeasonalityTransmission)
  
  if (alsoModelNoSeasonality)
    scenarios$RELATIVE_TRANSMISSIBILITY_AT_NADIR <- c(scenarios$RELATIVE_TRANSMISSIBILITY_AT_NADIR, 1.0)
}


# Mitigation --------------------------------------------------------------

mitigationPropertyGroup <- list()

loadMitigation <- function(mitigationPropertyName,
                           mitigationLabel,
                           mitigationNames,
                           mitigationValues) {
  
  # Load property covariation
  mitigationPropertyGroup[[mitigationPropertyName]]$name <<- mitigationPropertyName
  mitigationPropertyGroup[[mitigationPropertyName]]$labels <<- 
    c(mitigationPropertyGroup[[mitigationPropertyName]]$labels, 
      mitigationLabel)
  if (any(mitigationPropertyGroup[[mitigationPropertyName]]$properties != mitigationNames) && 
      length(mitigationPropertyGroup[[mitigationPropertyName]]$properties) != 0)
    stop("Mismatch of mitigation names")
  mitigationPropertyGroup[[mitigationPropertyName]]$properties <<- mitigationNames
  
  # Load scenarios
  for (i in 1:length(mitigationNames)) {
    scenarios[[mitigationNames[[i]]]] <<- c(
      scenarios[[mitigationNames[[i]]]],
      mitigationValues[[i]]
    )
  }
}

for (mitigationI in 1:length(mitigationDefinition)) {
  mitigation = mitigationDefinition[[mitigationI]]
  for (i in 1:nrow(mitigation)) {
    loadMitigation(
      names(mitigationDefinition)[[mitigationI]],
      mitigation$NAME[[i]], 
      names(select(mitigation, -NAME)),
      slice(select(mitigation, -NAME), i)
    )
  }
}

propertyGroups <- unname(c(propertyGroups, mitigationPropertyGroup))

# Read in and merge yaml specifications -----------------------------------

localOptions <- read_yaml("localOptions.yaml")
outputDir <- str_c(localOptions$outputDirectory, outputSubDir)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
localOptions$outputDirectory <- outputDir

reconstructedYaml <- c(
  # Local options that are likely to vary across analysts, will be stored by each analyst in a local yaml file
  localOptions,
  read_yaml("input/staticOptions.yaml"),
  read_yaml("input/reportOptions.yaml"),
  plugin_options,
  read_yaml("input/triggerOptions.yaml"),
  propertyGroups = list(propertyGroups),
  #TODO - check this - I don't think it is listed properly
  scenarios = list(#unname(
    c(scenarios,
      read_yaml("input/mitigationOptions.yaml"),
      read_yaml("input/miscConfigOptions.yaml"),
      read_yaml("input/seedingOptions.yaml")))
  #))
)

# Write YAML file ---------------------------------------------------------

write_yaml(x = reconstructedYaml,
           file = yamlFileName,
           indent.mapping.sequence = TRUE)
