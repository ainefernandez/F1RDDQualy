library(dplyr)
library(tidyverse)
library(stargazer)
library(rdrobust)
library(rdd)
library(lfe)

run_rd_felm <- function(data, outcome, qualifying_round = "Q1",
                        xlim_vals = NULL, ylim_vals = NULL,
                        p = 1) {
  
  # Define running and treatment variables
  if (qualifying_round == "Q1") {
    runningvar <- "GapToKnockoutQ1"        # running variable
    treat <- "MadeItToQ2"            # treatment indicator
  } else if (qualifying_round == "Q2") {
    runningvar <- "GapToKnockoutQ2"
    treat <- "MadeItToQ3"
  } else {
    stop("Qualifying round must be 'Q1' or 'Q2'")
  }
  
  # Prepare data
  vars_needed <- c(outcome, runningvar, treat, "Team", "GP")
  df <- data[complete.cases(data[, vars_needed]), ]
  
  # Bandwidth selection for RD
  bw <- rdbwselect(y = df[[outcome]], x = df[[runningvar]], c = 0, bwselect = "mserd")
  bandwidth <- bw$bws[1]
  
  # Filter data to bandwidth window
  df_filtered <- df[df[[runningvar]] >= -bandwidth & df[[runningvar]] <= bandwidth, ]
  
  # Formula for FELM with running variable and interaction
  fml <- as.formula(
    paste0(outcome, " ~ ", treat, " + ", runningvar, " + ", runningvar, ":", treat, " | Team:GP")
  )
  
  # Estimate model with fixed effects
  model <- felm(fml, data = df_filtered)
  
  # Print results
  print(summary(model))
  cat("Observations used:", nobs(model), "\n\n")
  
  # Plot RD using rdplot
  rdplot(
    y = df_filtered[[outcome]],
    x = df_filtered[[runningvar]],
    c = 0,
    p = p,
    x.label = runningvar,
    y.label = outcome,
    x.lim = xlim_vals,
    y.lim = ylim_vals,
    title = paste("RD Plot for", outcome, "around", qualifying_round)
  )
  
  return(model)
}

data<-read.csv("FinalDataSet.csv")
data$Team <- as.factor(data$Team)
data$GP <- as.factor(data$GP)

#OUTCOMES 

# For Grid Position
run_rd_felm(data, "GridPosition", "Q1", ylim_vals = c(5, 20), xlim_vals = c(-0.5, 0.5))
run_rd_felm(data, "GridPosition", "Q2", ylim_vals = c(4, 16), xlim_vals = c(-0.5, 0.5))

# For Race Position
run_rd_felm(data, "RacePosition", "Q1", ylim_vals = c(5, 20), xlim_vals = c(-1,1))
run_rd_felm(data, "RacePosition", "Q2", ylim_vals = c(4, 16), xlim_vals = c(-0.5, 0.5))

# For Points
run_rd_felm(data, "Points", "Q1", ylim_vals = c(0, 25), xlim_vals = c(-1,1))
run_rd_felm(data, "Points", "Q2", ylim_vals = c(0, 25), xlim_vals = c(-.4,.4))

# For DNF (Did Not Finish)
run_rd_felm(data, "DNF", "Q1", ylim_vals = c(0, 1), xlim_vals = c(-0.5, 0.5))
run_rd_felm(data, "DNF", "Q2", ylim_vals = c(0, 1), xlim_vals = c(-.6,.6))

# For DNF Driving (Did Not Finish due to a driving incident)
run_rd_felm(data, "DNFDriving", "Q1")
run_rd_felm(data, "DNFDriving", "Q2")

# For DNF Mechanical (Did Not Finish due to a mechanical issue)
run_rd_felm(data, "DNFMechanical", "Q1")
run_rd_felm(data, "DNFMechanical", "Q2")

#For StartingTyre
run_rd_felm(data, "TyreSOFT", "Q1", ylim_vals = c(0, .5), xlim_vals = c(-1,1))
run_rd_felm(data, "TyreSOFT", "Q2", ylim_vals = c(0, .6), xlim_vals = c(-.6,.6))

run_rd_felm(data, "TyreMEDIUM", "Q1", ylim_vals = c(0, 1), xlim_vals = c(-1,1))
run_rd_felm(data, "TyreMEDIUM", "Q2", ylim_vals = c(.25, .9), xlim_vals = c(-.6,.6))

run_rd_felm(data, "TyreHARD", "Q1", ylim_vals = c(0, .5), xlim_vals = c(-1,1))
run_rd_felm(data, "TyreHARD", "Q2", ylim_vals = c(0, .25), xlim_vals = c(-.6,.6))

# For Number of Pit Stops
run_rd_felm(data, "NumberOfPitStops", "Q1", ylim_vals = c(.5, 2), xlim_vals = c(-1,1))
run_rd_felm(data, "NumberOfPitStops", "Q2", ylim_vals = c(.5, 2), xlim_vals = c(-.6,.6))

# For Alternative Strategy 
run_rd_felm(data, "AlternativeStrategy", "Q1", ylim_vals = c(.5, 1), xlim_vals = c(-1,1))
run_rd_felm(data, "AlternativeStrategy", "Q2", ylim_vals = c(.4, 1), xlim_vals = c(-.6,.6))

# For Pitlane Start 
run_rd_felm(data, "Pitlane", "Q1")
run_rd_felm(data, "Pitlane", "Q2")

#COVARIATES 

# For Gap to Fastest FP1
run_rd_felm(data, "GapToFastestFP1", "Q1", ylim_vals = c(0, 10), xlim_vals = c(-1.25,1.25))
run_rd_felm(data, "GapToFastestFP1", "Q2", ylim_vals = c(0, 5), xlim_vals = c(-1.25,1.25))

# For Gap to Fastest FP2
run_rd_felm(data, "GapToFastestFP2", "Q1", ylim_vals = c(0, 5), xlim_vals = c(-1.25,1.25))
run_rd_felm(data, "GapToFastestFP2", "Q2", ylim_vals = c(0, 4), xlim_vals = c(-.3,.3))

# For Gap to Fastest FP3
run_rd_felm(data, "GapToFastestFP3", "Q1", ylim_vals = c(0, 4), xlim_vals = c(-0.5, 0.5))
run_rd_felm(data, "GapToFastestFP3", "Q2", ylim_vals = c(0, 3), xlim_vals = c(-0.5, 0.5))

# For Championship Points
run_rd_felm(data, "ChampionshipPoints", "Q1", ylim_vals = c(0, 100) , xlim_vals = c(-0.6, 0.6))
run_rd_felm(data, "ChampionshipPoints", "Q2", ylim_vals = c(0, 150), xlim_vals = c(-0.5, 0.5))

# For Team Championship Points
run_rd_felm(data, "TeamChampionshipPoints", "Q1", ylim_vals = c(0, 200), xlim_vals = c(-.75,.75))
run_rd_felm(data, "TeamChampionshipPoints", "Q2", ylim_vals = c(0, 250), xlim_vals = c(-0.5, 0.5))

# For Last Race Position
run_rd_felm(data, "LastRacePosition", "Q1", ylim_vals = c(5, 20), xlim_vals = c(-1, 1))
run_rd_felm(data, "LastRacePosition", "Q2", ylim_vals = c(5, 20), xlim_vals = c(-0.5, 0.5))

# For Teammate Last Race Position
run_rd_felm(data, "TeammateLastRacePosition", "Q1", ylim_vals = c(5, 20), xlim_vals = c(-1, 1))
run_rd_felm(data, "TeammateLastRacePosition", "Q2", ylim_vals = c(5, 20), xlim_vals = c(-0.5, 0.5))


#COVARIATE ANALYSIS 


covariate_balance <- function(data) {
  # Create lists to store results
  results_q1 <- list()
  results_q2 <- list()
  
  # Define covariates
  covariates <- c("GapToFastestFP1", "GapToFastestFP2", "GapToFastestFP3", 
                  "ChampionshipPoints", "TeamChampionshipPoints", 
                  "LastRacePosition", "TeammateLastRacePosition")
  
  for (var in covariates) {
    # Model for MadeItToQ2 (Q1)
    cutoff <- 0
    bw <- rdbwselect(y = data[[var]], x = data$GapToKnockoutQ1, c = cutoff, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    data_filtered_q1 <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
    
    # Fit model with covariates
    model_q1 <- felm(as.formula(paste(var, "~ MadeItToQ2 + GapToKnockoutQ1 | Team:GP")), data = data_filtered_q1)
    
    # Extract results
    coefs_q1 <- summary(model_q1)$coefficients
    results_q1[[var]] <- data.frame(
      Covariate = var,
      Treatment = "MadeItToQ2",
      Estimate = coefs_q1["MadeItToQ2", "Estimate"],
      p_value = coefs_q1["MadeItToQ2", "Pr(>|t|)"],
      CI_lower = coefs_q1["MadeItToQ2", "Estimate"] - 1.96 * coefs_q1["MadeItToQ2", "Std. Error"],
      CI_upper = coefs_q1["MadeItToQ2", "Estimate"] + 1.96 * coefs_q1["MadeItToQ2", "Std. Error"],
      Observations = nrow(data_filtered_q1),
      Bandwidth = bandwidth
    )
    
    # Model for MadeItToQ3 (Q2)
    bw <- rdbwselect(y = data[[var]], x = data$GapToKnockoutQ2, c = cutoff, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    data_filtered_q2 <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
    
    # Fit model with covariates
    model_q2 <- felm(as.formula(paste(var, "~ MadeItToQ3 + GapToKnockoutQ2 | Team:GP")), data = data_filtered_q2)
    
    # Extract results
    coefs_q2 <- summary(model_q2)$coefficients
    results_q2[[var]] <- data.frame(
      Covariate = var,
      Treatment = "MadeItToQ3",
      Estimate = coefs_q2["MadeItToQ3", "Estimate"],
      p_value = coefs_q2["MadeItToQ3", "Pr(>|t|)"],
      CI_lower = coefs_q2["MadeItToQ3", "Estimate"] - 1.96 * coefs_q2["MadeItToQ3", "Std. Error"],
      CI_upper = coefs_q2["MadeItToQ3", "Estimate"] + 1.96 * coefs_q2["MadeItToQ3", "Std. Error"],
      Observations = nrow(data_filtered_q2),
      Bandwidth = bandwidth
    )
  }
  
  # Combine results into data frames
  df_results_q1 <- do.call(rbind, results_q1)
  df_results_q2 <- do.call(rbind, results_q2)
  
  return(list(MadeItToQ2 = df_results_q1, MadeItToQ3 = df_results_q2))
}

# Example usage
results <- covariate_balance(data)

# Plot: RD Placebo Analysis (Q1)
ggplot(resultados$MadeItToQ2, aes(x = Estimate, y = reorder(Covariate, Estimate))) +
  geom_point(color = "#1f78b4", size = 3) +  # Points for the estimates
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#1f78b4") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical line at x = 0
  labs(
    title = "RD Placebo Analysis (Q1)",
    x = "RD Estimator",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 14)  
  )

# Plot: RD Placebo Analysis (Q2)
ggplot(resultados$MadeItToQ3, aes(x = Estimate, y = reorder(Covariate, Estimate))) +
  geom_point(color = "#1f78b4", size = 3) +  # Points for the estimates
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#1f78b4") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical line at x = 0
  labs(
    title = "RD Placebo Analysis (Q2)",
    x = "RD Estimator",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 14)  
  )
