# 2. Robustness Analysis
# This R script performs the robustness analysis for the RDD project.
# First, it tests different bandwidths, and then it evaluates different cutoffs for each outcome.
# The analysis is conducted separately for Q1 and Q2.


library(lfe)
library(dplyr)
library(ggplot2)


data<-read.csv("FinalDataSet.csv")
data$Team <- as.factor(data$Team)
data$GP <- as.factor(data$GP)

#BANWIDTHS

run_models_and_plotQ1 <- function(data, outcome, cutoff = 0, bandwidth_values = seq(0.05, 1.05, by = 0.1)) {
  results <- data.frame()  # Create a dataframe to store the results
  
  for (bw in bandwidth_values) {
    # Filter the data based on the current bandwidth
    data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bw) & data$GapToKnockoutQ1 <= (cutoff + bw), ]
    
    # Create the dynamic formula for the model
    formula <- as.formula(paste(outcome, "~ MadeItToQ2 + GapToKnockoutQ1 + GapToKnockoutQ1:MadeItToQ2 | Team:GP"))
    
    # Fit the model
    model <- felm(formula, data = data_filtered)
    
    # Extract coefficients and standard errors for MadeItToQ2 only
    if ("MadeItToQ2" %in% rownames(summary(model)$coefficients)) {
      estimate <- summary(model)$coefficients["MadeItToQ2", "Estimate"]
      std_error <- summary(model)$coefficients["MadeItToQ2", "Std. Error"]
      
      # Compute confidence intervals
      ci_lower <- estimate - 1.96 * std_error
      ci_upper <- estimate + 1.96 * std_error
      
      # Add results to the dataframe
      results <- rbind(results, data.frame(
        Outcome = outcome,
        Bandwidth = bw,
        Estimate = estimate,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      ))
    }
  }
  
  # Create the confidence interval plot for the MadeItToQ2 estimates
  ggplot(results, aes(x = Bandwidth, y = Estimate)) +
    geom_point(color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.05, color = "steelblue") +
    scale_x_continuous(limits = c(0.05, 1.05), breaks = seq(0.05, 1.05, by = 0.1)) +
    labs(x = "Bandwidth (GapToKnockoutQ1)",
         y = "Estimate") +
    theme_bw()
}

# Function to model and plot confidence intervals for MadeItToQ3
run_models_and_plotQ2 <- function(data, outcome, cutoff = 0, bandwidth_values = seq(0.05, 1.05, by = 0.1)) {
  results <- data.frame()  # Create a dataframe to store the results
  
  for (bw in bandwidth_values) {
    # Filter the data based on the current bandwidth
    data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bw) & data$GapToKnockoutQ2 <= (cutoff + bw), ]
    
    # Create the dynamic formula for the model
    formula <- as.formula(paste(outcome, "~ MadeItToQ3 + GapToKnockoutQ2 + GapToKnockoutQ2:MadeItToQ3 | Team:GP"))
    
    # Fit the model
    model <- felm(formula, data = data_filtered)
    
    # Extract coefficients and standard errors for MadeItToQ3 only
    if ("MadeItToQ3" %in% rownames(summary(model)$coefficients)) {
      estimate <- summary(model)$coefficients["MadeItToQ3", "Estimate"]
      std_error <- summary(model)$coefficients["MadeItToQ3", "Std. Error"]
      
      # Compute confidence intervals
      ci_lower <- estimate - 1.96 * std_error
      ci_upper <- estimate + 1.96 * std_error
      
      # Add results to the dataframe
      results <- rbind(results, data.frame(
        Outcome = outcome,
        Bandwidth = bw,
        Estimate = estimate,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      ))
    }
  }
  
  # Create the confidence interval plot for the MadeItToQ3 estimates
  ggplot(results, aes(x = Bandwidth, y = Estimate)) +
    geom_point(color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.05, color = "steelblue") +
    scale_x_continuous(limits = c(0.05, 1.05), breaks = seq(0.05, 1.05, by = 0.1)) +
    labs(x = "Bandwidth (GapToKnockoutQ2)",
         y = "Estimate") +
    theme_bw()
}

#GridPosition
run_models_and_plotQ1(data, "GridPosition")
run_models_and_plotQ2(data, "GridPosition")

#RacePosition
run_models_and_plotQ1(data, "RacePosition")
run_models_and_plotQ2(data, "RacePosition")

#Points
run_models_and_plotQ1(data, "Points")
run_models_and_plotQ2(data, "Points")

#DNF
run_models_and_plotQ1(data, "DNF")
run_models_and_plotQ2(data, "DNF")

#Alternative Strategy
run_models_and_plotQ1(data, "AlternativeStrategy")
run_models_and_plotQ2(data, "AlternativeStrategy")

#NumberOfPitstops
run_models_and_plotQ1(data, "NumberOfPitStops")
run_models_and_plotQ2(data, "NumberOfPitStops")

#TyreSOFT
run_models_and_plotQ1(data, "TireSOFT")
run_models_and_plotQ2(data, "TireSOFT")

#TyreMEDIUM
run_models_and_plotQ1(data, "TireMEDIUM")
run_models_and_plotQ2(data, "TireMEDIUM")

#TyreHARD
run_models_and_plotQ1(data, "TireHARD")
run_models_and_plotQ2(data, "TireHARD")

#PITLANE
run_models_and_plotQ1(data, "Pitlane")
run_models_and_plotQ2(data, "Pitlane")

#DNFdriving
run_models_and_plotQ1(data, "DNFDriving")
run_models_and_plotQ2(data, "DNFDriving")

#DNFMechanical
run_models_and_plotQ1(data, "DNFMechanical")
run_models_and_plotQ2(data, "DNFMechanical")


# CUTOFFS
cutoffQ1 <- function(data, outcome, cutoff_values = seq(-1, 1, by = 0.5)) {
  results <- data.frame()  # Create a dataframe to store the results
  
  for (cutoff in cutoff_values) {
    # Select the optimal bandwidth using rdbwselect
    bw <- rdbwselect(y = data[[outcome]], x = data$GapToKnockoutQ1, c = 0, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    
    # Filter the data according to the bandwidth and current cutoff
    data_filtered <- data[data$GapToKnockoutQ1 >= (cutoff - bandwidth) & 
                            data$GapToKnockoutQ1 <= (cutoff + bandwidth), ]
    
    # Create the dynamic formula for the model
    formula <- as.formula(paste(outcome, "~ MadeItToQ2 + GapToKnockoutQ1 + GapToKnockoutQ1:MadeItToQ2 | Team:GP"))
    
    # Fit the model
    model <- felm(formula, data = data_filtered)
    
    # Extract coefficients and standard errors for MadeItToQ2 only
    if ("MadeItToQ2" %in% rownames(summary(model)$coefficients)) {
      estimate <- summary(model)$coefficients["MadeItToQ2", "Estimate"]
      std_error <- summary(model)$coefficients["MadeItToQ2", "Std. Error"]
      
      # Calculate confidence intervals
      ci_lower <- estimate - 1.96 * std_error
      ci_upper <- estimate + 1.96 * std_error
      
      # Add results to the dataframe
      results <- rbind(results, data.frame(
        Outcome = outcome,
        Cutoff = cutoff,
        Estimate = estimate,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      ))
    }
  }
  
  # Create the confidence interval plot for the MadeItToQ2 estimates
  ggplot(results, aes(x = Cutoff, y = Estimate)) +
    geom_point(color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.05, color = "steelblue") +
    labs(x = "Cutoff (GapToKnockoutQ1)", y = "Estimate") +
    theme_bw()
}

cutoffQ2 <- function(data, outcome, cutoff_values = seq(-0.2, 0.2, by = 0.1)) {
  results <- data.frame()  # Create a dataframe to store the results
  
  for (cutoff in cutoff_values) {
    # Select the optimal bandwidth using rdbwselect
    bw <- rdbwselect(y = data[[outcome]], x = data$GapToKnockoutQ2, c = 0, bwselect = "mserd")
    bandwidth <- bw$bws[1]
    
    # Filter the data according to the bandwidth and current cutoff
    data_filtered <- data[data$GapToKnockoutQ2 >= (cutoff - bandwidth) & 
                            data$GapToKnockoutQ2 <= (cutoff + bandwidth), ]
    
    # Create the dynamic formula for the model
    formula <- as.formula(paste(outcome, "~ MadeItToQ3 + GapToKnockoutQ2 + GapToKnockoutQ2:MadeItToQ3 | Team:GP"))
    
    # Fit the model
    model <- felm(formula, data = data_filtered)
    
    # Extract coefficients and standard errors for MadeItToQ3 only
    if ("MadeItToQ3" %in% rownames(summary(model)$coefficients)) {
      estimate <- summary(model)$coefficients["MadeItToQ3", "Estimate"]
      std_error <- summary(model)$coefficients["MadeItToQ3", "Std. Error"]
      
      # Calculate confidence intervals
      ci_lower <- estimate - 1.96 * std_error
      ci_upper <- estimate + 1.96 * std_error
      
      # Add results to the dataframe
      results <- rbind(results, data.frame(
        Outcome = outcome,
        Cutoff = cutoff,
        Estimate = estimate,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      ))
    }
  }
  
  # Create the confidence interval plot for the MadeItToQ3 estimates
  ggplot(results, aes(x = Cutoff, y = Estimate)) +
    geom_point(color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.05, color = "steelblue") +
    labs(x = "Cutoff (GapToKnockoutQ2)", y = "Estimate") +
    theme_bw()
}

#GridPosition
cutoffQ1(data, "GridPosition")
cutoffQ2(data, "GridPosition")

#RacePosition
run_models_and_plotQ1(data, "RacePosition")
run_models_and_plotQ2(data, "RacePosition")

#Points
run_models_and_plotQ1(data, "Points")
run_models_and_plotQ2(data, "Points")

#DNF
run_models_and_plotQ1(data, "DNF")
run_models_and_plotQ2(data, "DNF")

#Alternative Strategy
run_models_and_plotQ1(data, "AlternativeStrategy")
run_models_and_plotQ2(data, "AlternativeStrategy")

#NumberOfPitstops
run_models_and_plotQ1(data, "NumberOfPitStops")
run_models_and_plotQ2(data, "NumberOfPitStops")

#TyreSOFT
run_models_and_plotQ1(data, "TireSOFT")
run_models_and_plotQ2(data, "TireSOFT")

#TyreMEDIUM
run_models_and_plotQ1(data, "TireMEDIUM")
run_models_and_plotQ2(data, "TireMEDIUM")

#TyreHARD
run_models_and_plotQ1(data, "TireHARD")
run_models_and_plotQ2(data, "TireHARD")

#PITLANE
run_models_and_plotQ1(data, "Pitlane")
run_models_and_plotQ2(data, "Pitlane")

#DNFdriving
run_models_and_plotQ1(data, "DNFDriving")
run_models_and_plotQ2(data, "DNFDriving")

#DNFMechanical
run_models_and_plotQ1(data, "DNFMechanical")
run_models_and_plotQ2(data, "DNFMechanical")

#RD Robust
analysis_cutoff_rdrobust <- function(data, running_var, dependent_var, cutoffs = seq(-1, 1, by = 0.5), grado_pol = 1) {
  
  # Create a list to store results
  results <- list()
  
  # Iterate over each cutoff in the `cutoffs` vector
  for (cutoff in cutoffs) {
    # Check if the cutoff is within the range of the running variable
    if (cutoff < min(data[[running_var]], na.rm = TRUE) | cutoff > max(data[[running_var]], na.rm = TRUE)) {
      message(paste("The specified cutoff (", cutoff, ") is not within the range of the running variable:", running_var))
      next  # Skip this cutoff and move to the next
    }
    
    rd_result <- tryCatch(
      rdrobust(y = data[[dependent_var]], x = data[[running_var]], c = cutoff, p = grado_pol),
      error = function(e) {
        message(paste("Error in analysis with cutoff:", cutoff, "\n", e))
        return(NULL)
      }
    )
    
    # Check if there was an error in the model
    if (is.null(rd_result)) {
      next  # Move to the next cutoff if there's an error
    }
    
    # Extract relevant estimates and statistics
    estimator <- rd_result$Estimate[1]
    p_value <- rd_result$pv[3]
    ci_low <- rd_result$ci[3, 1]
    ci_high <- rd_result$ci[3, 2]
    bw_mse <- round(rd_result$bws[1], 3)
    obs_effective <- sum(rd_result$N_h)
    
    # Store results in the list
    results[[as.character(cutoff)]] <- data.frame(
      Cutoff = cutoff,
      MSE_Optimal_Bandwidth = bw_mse,
      RD_Estimator = round(estimator, 3),
      p_value = round(p_value, 3),
      CI_Lower = round(ci_low, 3),
      CI_Upper = round(ci_high, 3),
      Eff_Number_Observations = obs_effective
    )
  }
  
  # Combine all results into a single data frame
  results_df <- do.call(rbind, results)
  
  ggplot(results_df, aes(x = Cutoff, y = RD_Estimator)) +
    geom_point(color = "red", size = 2) +  # Red points for estimates
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "steelblue") +  # Vertical error bars in blue
    geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Horizontal line at y = 0
    scale_y_continuous(limits = c(min(results_df$CI_Lower), max(results_df$CI_Upper))) +  # Set dynamic limits for the y-axis
    labs(
      x = paste("Cutoff", "(", running_var, ")"),
      y = "RDD Estimate"
    )+
    theme_bw()
}

#Grid Position
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1","GridPosition")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2","GridPosition")

#RacePosition
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1","RacePosition")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2","RacePosition")

#Points
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1","Points")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2","Points")

#DNF
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1","DNF")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2","DNF")

# Alternative Strategy
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "AlternativeStrategy")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "AlternativeStrategy")

# Number of Pitstops
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "NumberOfPitStops")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "NumberOfPitStops")

# Tyre SOFT
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "TireSOFT")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "TireSOFT")

# Tyre MEDIUM
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "TireMEDIUM")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "TireMEDIUM")

# Tyre HARD
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "TireHARD")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "TireHARD")

# PITLANE
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "Pitlane")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "Pitlane")

# DNF driving
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "DNFDriving")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "DNFDriving")

# DNF Mechanical
analysis_cutoff_rdrobust(data, "GapToKnockoutQ1", "DNFMechanical")
analysis_cutoff_rdrobust(data, "GapToKnockoutQ2", "DNFMechanical")


