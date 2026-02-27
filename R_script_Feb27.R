library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(unmarked)
library(ggplot2)
library(tidyverse)
library(suncalc)
library(activity)
library(overlap)
library(circular)

###########################################################
##Section A: Load and Clean Data
###########################################################

# Survey periods
SURVEY_2018_START <- as.Date("2018-01-18")
SURVEY_2018_END <- as.Date("2018-11-16")

SURVEY_2021_START <- as.Date("2021-10-07")
SURVEY_2021_END <- as.Date("2022-06-07")

SURVEY_2024_START <- as.Date("2024-07-25")
SURVEY_2024_END <- as.Date("2025-03-25")

# Configuration
    # Species to analyze
    TARGET_SPECIES = c("Sunda Pangolin", "Lesser Mousedeer", "Common Treeshrew", "Long-tailed Macaque", "Common Palm Civet", "Sambar Deer")
    
    # Activity model settings
    bootstrap_reps = 999  # Number of bootstrap replicates (999 for final, 99 for quick testing)
    
# Load camera trap data
data_2018 <- read_excel("2018_raw_data.xlsx")
data_2021 <- read_excel("2021_raw_data.xlsx")
data_2024 <- read_excel("2024_raw_data.xlsx")

# Clean 2024 dataset time format
data_2024 <- data_2024 %>%
    mutate(
        time_char = sprintf("%06d", raw_time),
        time_hms  = paste0(
            substr(time_char, 1, 2), ":",
            substr(time_char, 3, 4), ":",
            substr(time_char, 5, 6)
        ),
        time = as.POSIXct(
            paste(date, time_hms),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "Asia/Singapore"
        )
    )


# Function to clean data and classify diel period
classify_diel_period <- function(data,
                                 survey_start,
                                 survey_end,
                                 lat,
                                 lon,
                                 tz = "Asia/Singapore") { 

data <- data %>%
    # Remove non-detection rows
    filter(
      !grepl("\\b(unidentified|unknown|human)\\b", species1ID, ignore.case = TRUE),
!grepl("\\?", species1ID, ignore.case = TRUE),
      !species1ID %in% c("Start", "End", "FALSE", "False", "false",
                         "START", "END", "Black", "Unknown", "unknown",
                         "Setup", "Pickup", "???", "FAULTY",
                         "file corrupted", "STAFF", "Staff",
                         "Unidentified", "end", "Human", "human", "Humans"),
      !is.na(species1ID),
      !is.na(date),
    ) %>%
    
    # Fix time by combining correct date with time
    mutate(
      time_fixed = as.POSIXct(
        paste(
          as.Date(date),
          format(time, "%H:%M:%S")
        ),
        tz = tz
      ),
date = as.Date(date)
    )

# Create sequence of survey dates
  dates <- seq(
    as.Date(survey_start),
    as.Date(survey_end),
    by = "day"
  )

# Get sunlight timings
  sun_times <- suncalc::getSunlightTimes(
    date = dates,
    lat  = 1.35,
    lon  = 103.82,
    tz   = tz,
    keep = c("dawn", "sunrise", "sunset", "dusk")
  )

# Join sunlight timings to the dataframe
  data <- data %>%
    left_join(sun_times, by = "date")

# Set crepuscular periods
  data <- data %>%
    mutate(
      dawn_minus_30   = dawn   - lubridate::minutes(30),
      sunset_minus_30 = sunset - lubridate::minutes(30)
    )

# Classify diel period
  data <- data %>%
    mutate(
      diel_period = case_when(
        time_fixed >= sunrise & time_fixed < sunset ~ "Day",
        time_fixed >= dawn_minus_30   & time_fixed < sunrise ~ "Civil Dawn",
        time_fixed >= sunset_minus_30 & time_fixed < dusk    ~ "Civil Dusk",
        TRUE                                                   ~ "Night"
      )
    )

# Convert time to radians
    data <- data %>%
        mutate(
            time_rad = gettime(time_fixed, format = "%H:%M:%S", scale = c("radian"))
        )

  return(data)
}

# Run function on camera trap data
detections_2018 <- classify_diel_period(
data = data_2018,
survey_start = SURVEY_2018_START,
survey_end = SURVEY_2018_END,
)
detections_2021 <- classify_diel_period(
data = data_2021,
survey_start = SURVEY_2021_START,
survey_end = SURVEY_2021_END,
)
detections_2024 <- classify_diel_period(
data = data_2024,
survey_start = SURVEY_2024_START,
survey_end = SURVEY_2024_END,
)

# Bind rows together
all_detections <- bind_rows(
     detections_2018 %>% mutate(Period = "2018") %>% select(date, time, species1ID, Period, diel_period, time_rad, dawn_minus_30, sunrise, sunset_minus_30, dusk),
     detections_2021 %>% mutate(Period = "2021") %>% select(date, time, species1ID, Period, diel_period, time_rad, dawn_minus_30, sunrise, sunset_minus_30, dusk),
     detections_2024 %>% mutate(Period = "2024") %>% select(date, time, species1ID, Period, diel_period, time_rad, dawn_minus_30, sunrise, sunset_minus_30, dusk)
 )


###########################################################
##Section B: Create Detection Histories for Number of Detection Days
###########################################################

# Count daily detections by 24-hour rule
detection_days_summary <- all_detections %>%
  distinct(date, species1ID, Period) %>%
  count(Period, species1ID, name = "detection_days")

# Create daily detection history
detection_histories <- list()

for (species_name in TARGET_SPECIES) {
  detection_histories[[species_name]] <- list()

# 2018 PERIOD

# Pull out detection dates
species_2018 <- detections_2018 %>%
    filter(species1ID == species_name) %>%
    pull(date) %>%
    as.Date()  

# Create list of all dates in the survey period
if (length(species_2018) > 0) {
    all_days_2018 <- seq(SURVEY_2018_START, SURVEY_2018_END, by = "day")

# 24-hour rule detection
detection_dates_2018 <- unique(species_2018)

# Create detection matrix
y_2018 <- as.integer(all_days_2018 %in% detection_dates_2018)

detection_histories[[species_name]][["2018"]] <- list(
      y = matrix(y_2018, nrow = 1),
      dates = all_days_2018,
      n_days = length(all_days_2018),
      n_detections = sum(y_2018)
    )

} else {
    detection_histories[[species_name]][["2018"]] <- NULL
}

# 2021 PERIOD

species_2021 <- detections_2021 %>%
    filter(species1ID == species_name) %>%
    pull(date) %>%
    as.Date()
  
  if (length(species_2021) > 0) {
    all_days_2021 <- seq(SURVEY_2021_START, SURVEY_2021_END, by = "day")
    
    detection_dates_2021 <- unique(species_2021)
    
    y_2021 <- as.integer(all_days_2021 %in% detection_dates_2021)
    
    detection_histories[[species_name]][["2021"]] <- list(
      y = matrix(y_2021, nrow = 1),
      dates = all_days_2021,
      n_days = length(all_days_2021),
      n_detections = sum(y_2021)
    )
    
  } else {
    detection_histories[[species_name]][["2021"]] <- NULL
    cat("    No detections\n")
  }


# 2024 PERIOD

  species_2024 <- detections_2024 %>%
    filter(species1ID == species_name) %>%
    pull(date) %>%
    as.Date()
  
  if (length(species_2024) > 0) {
    all_days_2024 <- seq(SURVEY_2024_START, SURVEY_2024_END, by = "day")
    
    detection_dates_2024 <- unique(species_2024)
    
    y_2024 <- as.integer(all_days_2024 %in% detection_dates_2024)
    
    detection_histories[[species_name]][["2024"]] <- list(
      y = matrix(y_2024, nrow = 1),
      dates = all_days_2024,
      n_days = length(all_days_2024),
      n_detections = sum(y_2024)
    )
    
  } else {
    detection_histories[[species_name]][["2024"]] <- NULL
    cat("    No detections\n")
  }
  }

###########################################################
##Section C: Detection Probability Modelling
###########################################################

results_all <- list()

for (species in TARGET_SPECIES) {
  
  results_all[[species]] <- list()
  
  for (period in c("2018", "2021", "2024")) {
    dh <- detection_histories[[species]][[period]]
    
    if (is.null(dh)) {
      cat(paste0("  ", period, ": No detections\n"))
      next
    }
        
    # Need at least 3 detections to fit model
    if (dh$n_detections < 3) {
      cat("    (Too few detections for modeling)\n")
      results_all[[species]][[period]] <- list(
        n_days = dh$n_days,
        n_detections = dh$n_detections,
        model_fit = FALSE
      )
      next
    }
    
    # Build occupancy model
    y <- dh$y
    day_number <- matrix(1:dh$n_days, nrow = 1, byrow = TRUE)
    umf <- unmarkedFrameOccu(y = y, obsCovs = list(day = day_number))
    
    # Fit model
    m_constant <- occu(~ 1 ~ 1, data = umf)  # Constant detection
    m_temporal <- tryCatch(
      occu(~ scale(day) ~ 1, data = umf),    # Temporal detection
      error = function(e) NULL
    )
    
    # Select best model
    if (!is.null(m_temporal)) {
      model_list <- fitList("constant" = m_constant, "temporal" = m_temporal)
      model_selection <- modSel(model_list)
      best_model_name <- as.character(model_selection@Full$model[1])
      best_model <- if (best_model_name == "temporal") m_temporal else m_constant
    } else {
      best_model <- m_constant
      best_model_name <- "constant"
    }
    
    # Calculate detection probability with SE and CI for both constant and varying detection probabilities
    if (best_model_name == "constant") {
      p_est <- backTransform(best_model, "det")
      p <- coef(p_est)
      p_se <- SE(p_est)
      p_ci <- c(
        plogis(qlogis(p) - 1.96 * (p_se / (p * (1 - p)))),
        plogis(qlogis(p) + 1.96 * (p_se / (p * (1 - p))))
      )
    } else {
      pred <- predict(best_model, type = "det", newdata = data.frame(day = mean(1:dh$n_days)))
      p <- pred$Predicted[1]
      p_se <- pred$SE[1]
      p_ci <- c(pred$lower[1], pred$upper[1])
    }
        
    results_all[[species]][[period]] <- list(
      n_days = dh$n_days,  
      n_detections = dh$n_detections,
      model_fit = TRUE,
      best_model = best_model_name,
      p = p,
      p_se = p_se,
      p_ci = p_ci
    )
  }
}

###########################################################
##Section D: Results Table
###########################################################

results_df <- data.frame()

for (species in TARGET_SPECIES) {
  for (period in c("2018", "2021", "2024")) {
    res <- results_all[[species]][[period]]
    if (!is.null(res)) {
      row <- data.frame(
        Species = species,
        Period = period,
        Survey_Days = res$n_days,
        Detection_Days = res$n_detections,
        Detection_Rate = round(res$n_detections / res$n_days, 3)
      )
      
      if (res$model_fit) {
        row$Best_Model = res$best_model
        row$Detection_Prob = round(res$p, 3)
        row$CI_Lower = round(res$p_ci[1], 3)
        row$CI_Upper = round(res$p_ci[2], 3)
      } else {
        row$Best_Model = "Not fitted"
        row$Detection_Prob = NA
        row$CI_Lower = NA
        row$CI_Upper = NA
      }
      
      results_df <- bind_rows(results_df, row)
    }
  }
}

write.csv(results_df,"results_df.csv")

###########################################################
##Section E: Plot Detection Probability Diagrams
###########################################################

plot_data <- results_df %>%
  filter(!is.na(Detection_Prob))

if (nrow(plot_data) > 0) {
  
  # Create summary figure
  plot_data <- plot_data %>%
    mutate(
      Period_Label = case_when(
        Period == "2018" ~ "2018\n(11 months)",
        Period == "2021" ~ "2021-2022\n(9 months)",
        Period == "2024" ~ "2024-2025\n(9 months)"
      )
    )
}

# Plot Detection Days
means_df <- plot_data %>%
    group_by(Species, Period_Label) %>%
    summarise(
        mean_days = mean(Detection_Days, na.rm = TRUE),
        .groups = "drop"
    )

ggplot(means_df, aes(x = Period_Label, 
                     y = mean_days, 
                     group = Species,
                     fill = Species,
                     colour = Species)) +
    
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_line(linewidth = 0.8, show.legend = FALSE) +
    geom_point(size = 2, show.legend = FALSE) +
    
    facet_wrap(~ Species, scales = "free_y") +
    
    labs(
        title = "Detection Days by Survey Period",
        x = "Survey Period",
        y = "Mean number of detection days"
    ) +
    
    theme_minimal() +
    theme(
        panel.grid = element_blank()
    )


# Plot Detection Probability
means_df <- plot_data %>%
    group_by(Species, Period_Label) %>%
    summarise(
        mean_prob = mean(Detection_Prob, na.rm = TRUE),
        CI_Lower = mean(CI_Lower, na.rm = TRUE),
        CI_Upper = mean(CI_Upper, na.rm = TRUE),
        .groups = "drop"
    )

ggplot(means_df, aes(x = Period_Label, 
                     y = mean_prob, 
                     group = Species,
                     fill = Species,
                     colour = Species)) +
    
    
    geom_col(width = 0.6) +
    
    geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0.2
    ) +
    
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    
    facet_wrap(~ Species, scales = "free_y") +
    
    scale_y_continuous(labels = scales::percent_format()) +
    
    labs(
        title = "Daily Detection Probability by Survey Period",
        x = "Survey Period",
        y = "Detection Probability"
    ) +
    
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        legend.position = "none"
    )


###########################################################
##Section F: Create Detection Histories for Day and Diel 
###########################################################

DIEL_LEVELS <- c("Civil Dawn", "Day", "Civil Dusk", "Night")

create_diel_detection_history <- function(species_name, detections_period, survey_start, survey_end) {
    
    # All survey days
    all_days <- seq(as.Date(survey_start), as.Date(survey_end), by = "day")
    n_days <- length(all_days)
    n_diel <- length(DIEL_LEVELS)
    
    # Filter detections for this species and period
    species_dets <- all_detections %>%
        filter(species1ID == species_name, period == detections_period)
    
    # If no detections at all, return NULL (keeps your existing logic)
    if (nrow(species_dets) == 0) return(NULL)
    
    # y matrix: n_days rows x 4 columns (one per diel period)
    y_matrix <- matrix(0, nrow = n_days, ncol = n_diel)
    colnames(y_matrix) <- DIEL_LEVELS
    
    for (i in seq_along(all_days)) {
        day_dets      <- species_dets %>% filter(as.Date(date) == all_days[i])
        diel_detected <- unique(day_dets$diel_period)
        for (dl in DIEL_LEVELS) {
            y_matrix[i, dl] <- as.integer(dl %in% diel_detected)
        }
    }
    
    # Observation-level covariates (n_days x 4 matrices)
    # diel: which diel period each column represents
    diel_covariate <- matrix(
        rep(DIEL_LEVELS, each = n_days),
        nrow = n_days, ncol = n_diel
    )
    colnames(diel_covariate) <- DIEL_LEVELS
    
    # day: survey day number (for temporal trend)
    day_covariate <- matrix(
        rep(seq_len(n_days), times = n_diel),
        nrow = n_days, ncol = n_diel
    )
    colnames(day_covariate) <- DIEL_LEVELS
    
    n_detections <- sum(y_matrix)
    
    cat(paste0("  ", detections_period, ": ", n_days, " survey days, ",
               n_detections, " diel-period detections\n"))
    cat(paste0("    Civil Dawn: ", sum(y_matrix[, "Civil Dawn"]), " days | ",
               "Day: ",         sum(y_matrix[, "Day"]),        " days | ",
               "Civil Dusk: ",        sum(y_matrix[, "Civil Dusk"]),       " days | ",
               "Night: ",       sum(y_matrix[, "Night"]),      " days\n"))
    
    
    return(list(
        y              = y_matrix,
        diel_covariate = diel_covariate,
        day_covariate  = day_covariate,
        all_days    = all_days,
        n_days         = n_days,
        n_detections   = n_detections
    ))
}
# Survey period definitions
survey_periods <- list(
    "2018" = list(start = SURVEY_2018_START, end = SURVEY_2018_END),
    "2021" = list(start = SURVEY_2021_START, end = SURVEY_2021_END),
    "2024" = list(start = SURVEY_2024_START, end = SURVEY_2024_END)
)

# Build detection histories for all species and periods 

diel_histories <- list()

for (species in TARGET_SPECIES) {
    cat(paste0("Building diel detection history for ", species, "...\n"))
    diel_histories[[species]] <- list()
    
    for (period in c("2018", "2021", "2024")) {
        dh <- create_diel_detection_history(
            species_name  = species,
            detections_period  = period,
            survey_start  = survey_periods[[period]]$start,
            survey_end    = survey_periods[[period]]$end
        )
        
        if (!is.null(dh)) {
            diel_histories[[species]][[period]] <- dh
        } else {
            cat(paste0("  ", period, ": No detections\n"))
        }
    }
    cat("\n")
}

# Fit Occupancy Models by Diel Period
diel_results <- list()

for (species in TARGET_SPECIES) {
    cat(paste0("--- ", species, " ---\n"))
    diel_results[[species]] <- list()
    
    for (period in c("2018", "2021", "2024")) {
        dh <- diel_histories[[species]][[period]]
        
        if (is.null(dh) || dh$n_detections < 3) {
            cat(paste0("  ", period, ": Skipping (insufficient detections)\n"))
            next
        }
        
        # Create unmarked frame with diel and day temporal covariates
        umf <- tryCatch({
            unmarkedFrameOccu(
                y       = dh$y,
                obsCovs = list(
                    diel = as.data.frame(dh$diel_covariate),
                    day  = as.data.frame(dh$day_covariate)
                )
            )
        }, error = function(e) {
            cat(paste0("  ", period, ": Failed to create unmarked frame - ", e$message, "\n"))
            return(NULL)
        })
        
        if (is.null(umf)) next
        
        # Fit 4 candidate models with scale for standardised day
        m_constant  <- tryCatch(occu(~ 1                  ~ 1, data = umf), error = function(e) NULL)
        m_diel      <- tryCatch(occu(~ diel                ~ 1, data = umf), error = function(e) NULL)
        m_temporal  <- tryCatch(occu(~ scale(day)          ~ 1, data = umf), error = function(e) NULL)
        m_diel_temp <- tryCatch(occu(~ diel + scale(day)   ~ 1, data = umf), error = function(e) NULL)
        
        if (is.null(m_constant)) {
            cat(paste0("  ", period, ": All models failed\n"))
            next
        }
        
        # Build list from converged models only
        fitted_models <- list()
        if (!is.null(m_constant))  fitted_models[["constant"]]     <- m_constant
        if (!is.null(m_diel))      fitted_models[["diel"]]          <- m_diel
        if (!is.null(m_temporal))  fitted_models[["temporal"]]      <- m_temporal
        if (!is.null(m_diel_temp)) fitted_models[["diel_temporal"]] <- m_diel_temp
        
        fl        <- do.call(fitList, fitted_models)
        ms        <- modSel(fl)
        best_name <- as.character(ms@Full$model[1])
        best_model <- fitted_models[[best_name]]
        
        # AIC table
        aic_table <- data.frame(
            model = names(fitted_models),
            AIC   = sapply(fitted_models, function(m) m@AIC)
        ) %>% arrange(AIC) %>% mutate(delta_AIC = AIC - min(AIC))
        
        cat(paste0("  ", period, ": Best model = ", best_name,
                   " (AIC = ", round(min(aic_table$AIC), 2), ")\n"))
        for (j in 1:nrow(aic_table)) {
            cat(paste0("    ", aic_table$model[j], ": AIC=", round(aic_table$AIC[j], 2),
                       ", ΔAIC=", round(aic_table$delta_AIC[j], 2), "\n"))
        }
        
        # Predict detection for all 4 diel periods (at mean survey day)
        mean_day   <- mean(seq_len(dh$n_days))
        pred_diel  <- list()
        
        for (dl in DIEL_LEVELS) {
            pred_diel[[dl]] <- tryCatch({
                nd   <- data.frame(diel = dl, day = mean_day)
                pred <- predict(best_model, type = "det", newdata = nd)
                list(p = pred$Predicted[1], lower = pred$lower[1], upper = pred$upper[1])
            }, error = function(e) {
                tryCatch({
                    nd   <- data.frame(diel = dl)
                    pred <- predict(best_model, type = "det", newdata = nd)
                    list(p = pred$Predicted[1], lower = pred$lower[1], upper = pred$upper[1])
                }, error = function(e2) NULL)
            })
            if (!is.null(pred_diel[[dl]])) {
                cat(paste0("    p(", dl, ") = ", round(pred_diel[[dl]]$p, 3),
                           " (95% CI: ", round(pred_diel[[dl]]$lower, 3),
                           "-",          round(pred_diel[[dl]]$upper, 3), ")\n"))
            }
        }
        
        p_constant <- tryCatch({
            bt <- backTransform(m_constant, "det")
            list(p = coef(bt), se = SE(bt))
        }, error = function(e) NULL)
        if (!is.null(p_constant)) cat(paste0("    p(constant) = ", round(p_constant$p, 3), "\n"))
        cat("\n")
        
        diel_results[[species]][[period]] <- list(
            fitted_models = fitted_models,
            best_model    = best_name,
            aic_table     = aic_table,
            pred_diel     = pred_diel,
            p_constant    = p_constant,
            n_detections  = dh$n_detections,
            n_days        = dh$n_days
        )
    }
}

# Build results table
results_df <- data.frame()

for (species in TARGET_SPECIES) {
    for (period in c("2018", "2021", "2024")) {
        res <- diel_results[[species]][[period]]
        if (is.null(res)) next
        
        # Helper to safely extract prediction values
        get_p <- function(dl, what) {
            val <- res$pred_diel[[dl]][[what]]
            ifelse(is.null(val) || is.na(val), NA, round(val, 3))
        }
        
        row <- data.frame(
            Species      = species,
            Period       = period,
            N_Days       = res$n_days,
            N_Detections = res$n_detections,
            Best_Model   = res$best_model,
            p_constant   = ifelse(!is.null(res$p_constant), round(res$p_constant$p, 3), NA),
            
            p_Civil_Dawn       = get_p("Civil Dawn", "p"),
            p_Civil_Dawn_lower = get_p("Civil Dawn", "lower"),
            p_Civil_Dawn_upper = get_p("Civil Dawn", "upper"),
            
            p_Day        = get_p("Day", "p"),
            p_Day_lower  = get_p("Day", "lower"),
            p_Day_upper  = get_p("Day", "upper"),
            
            p_Civil_Dusk       = get_p("Civil Dusk", "p"),
            p_Civil_Dusk_lower = get_p("Civil Dusk", "lower"),
            p_Civil_Dusk_upper = get_p("Civil Dusk", "upper"),
            
            p_Night       = get_p("Night", "p"),
            p_Night_lower = get_p("Night", "lower"),
            p_Night_upper = get_p("Night", "upper")
        )
        
        results_df <- bind_rows(results_df, row)
    }
}

write_csv(results_df, "diel_detection_results.csv")

# Plot Detection Probability by Diel Period
plot_data <- results_df %>%
    filter(!is.na(p_Day)) %>%
    pivot_longer(
        cols      = c(p_Civil_Dawn, p_Day, p_Civil_Dusk, p_Night),
        names_to  = "Diel_Period",
        values_to = "Detection_Prob"
    ) %>%
    mutate(
        CI_Lower = case_when(
            Diel_Period == "p_Civil_Dawn" ~ p_Civil_Dawn_lower,
            Diel_Period == "p_Day"        ~ p_Day_lower,
            Diel_Period == "p_Dusk"       ~ p_Civil_Dusk_lower,
            Diel_Period == "p_Night"      ~ p_Night_lower
        ),
        CI_Upper = case_when(
            Diel_Period == "p_Civil_Dawn" ~ p_Civil_Dawn_upper,
            Diel_Period == "p_Day"        ~ p_Day_upper,
            Diel_Period == "p_Civil_Dusk"       ~ p_Civil_Dusk_upper,
            Diel_Period == "p_Night"      ~ p_Night_upper
        ),
        Diel_Period = gsub("p_", "", Diel_Period),
        Diel_Period = gsub("_", " ", Diel_Period),
        Diel_Period = factor(Diel_Period, levels = c("Civil Dawn", "Day", "Civil Dusk", "Night"))
    ) %>%
    select(Species, Period, Diel_Period, Detection_Prob, CI_Lower, CI_Upper) %>%
    filter(!is.na(Detection_Prob))

if (nrow(plot_data) > 0) {
    
    p1 <- ggplot(plot_data, aes(x = Diel_Period, y = Detection_Prob, fill = Diel_Period)) +
        geom_col(width = 0.6) +
        geom_errorbar(
            aes(ymin = CI_Lower, ymax = CI_Upper),
            width = 0.05, linewidth = 0.1, na.rm = TRUE
        ) +
        facet_grid(Species ~ Period,
                   labeller = labeller(
                       Species = label_wrap_gen(width = 10),
                       Period  = label_wrap_gen(width = 12)
                   )) +
        scale_fill_manual(
            values = c(
                "Civil Dawn" = "#F4A460",
                "Day"        = "#FFD700",
                "Civil Dusk"       = "#FF8C00",
                "Night"      = "#4B0082"
            )
        ) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        labs(
            title    = "Detection Probability by Diel Period",
            subtitle = "Occupancy model: best model selected by AIC",
            x        = "Diel Period",
            y        = "Detection Probability",
            fill     = "Diel Period"
        ) +
        theme_bw() +
        theme(
            panel.grid = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA)
        )+
        theme(
            plot.title      = element_text(size = 14, face = "bold"),
            legend.position = "bottom",
            axis.text.x     = element_blank()
        )
    
   
        plot(p1)
    
}
###########################################################
##Section G: Chi-Square Test for Diel Shift
###########################################################

diel_tests <- list()

for (species in TARGET_SPECIES) {
    
    cat(paste0("Testing diel period shifts for ", species, "...\n"))
    
    # Get data for this species
    species_diel <- all_detections %>%
        filter(species1ID == species) %>%
	distinct(date, Period, diel_period) %>% #24hr-rule
        select(Period, diel_period)
    
    if (nrow(species_diel) < 20) {
        cat("  Insufficient data\n\n")
        next
    }
    
    # Create contingency table
    diel_table <- table(species_diel$Period, species_diel$diel_period)
    
    cat("\nContingency table:\n")
    print(diel_table)
    cat("\n")
    
    # Chi-square test
    chi_test <- chisq.test(diel_table)
    
    cat(paste0("Chi-square test: X² = ", round(chi_test$statistic, 3), 
               ", df = ", chi_test$parameter, 
               ", p = ", round(chi_test$p.value, 4), "\n"))
    
    if (chi_test$p.value < 0.05) {
        cat("*** Significant difference in diel period use across years\n")
    } else {
        cat("No significant difference in diel period use across years\n")
    }
    
    # Calculate proportions
    diel_props <- prop.table(diel_table, 1) * 100
    cat("\nProportions (%):\n")
    print(round(diel_props, 1))
    
    diel_tests[[species]] <- list(
        table = diel_table,
        chi_test = chi_test,
        proportions = diel_props
    )
    
    cat("\n")
}

##########################################################
##Section H: Circular Kernel Density Model
###########################################################

sp <- "Lesser Mousedeer"   # change as needed
# Create activity curves in radians
t18 <- all_detections %>% filter(species1ID == sp, Period == 2018) %>% pull(time_rad)
t21 <- all_detections %>% filter(species1ID == sp, Period == 2021) %>% pull(time_rad)
t24 <- all_detections %>% filter(species1ID == sp, Period == 2024) %>% pull(time_rad)

# Create dawn/dusk boundaries with mean values
bounds <- all_detections %>%
    filter(species1ID == sp) %>%
    summarise(
        dawn_start  = mean(hour(dawn_minus_30)   + minute(dawn_minus_30)/60 + second(dawn_minus_30)/3600, na.rm = TRUE),
        dawn_end    = mean(hour(sunrise)         + minute(sunrise)/60       + second(sunrise)/3600, na.rm = TRUE),
        dusk_start  = mean(hour(sunset_minus_30) + minute(sunset_minus_30)/60 + second(sunset_minus_30)/3600, na.rm = TRUE),
        dusk_end    = mean(hour(dusk)            + minute(dusk)/60          + second(dusk)/3600, na.rm = TRUE)
    )

dawn_start <- bounds$dawn_start
dawn_end   <- bounds$dawn_end
dusk_start <- bounds$dusk_start
dusk_end   <- bounds$dusk_end

# Plot overlay curves

xlim_plot <- c(0, 24)

# Compute densities
d18 <- densityPlot(t18, xscale = 24, plot = FALSE)
d21 <- densityPlot(t21, xscale = 24, plot = FALSE)
d24 <- densityPlot(t24, xscale = 24, plot = FALSE)

# Function to trim x-scale to 0–24 range
trim_density <- function(d) {
    keep <- d$x >= 0 & d$x <= 24
    list(x = d$x[keep], y = d$y[keep])
}

d18t <- trim_density(d18)
d21t <- trim_density(d21)
d24t <- trim_density(d24)

# Set y-limit
ylim_plot <- c(0, max(c(d18t$y, d21t$y, d24t$y), na.rm = TRUE) * 1.05)

# Plot curves
par(xaxs = "i")

plot(d18t$x, d18t$y, type = "l",
     col = "black", lwd = 2,
     xlim = xlim_plot,
     ylim = ylim_plot,
     main = paste0(sp, " — activity density (2018/2021/2024)"),
     xlab = "Time of day (hours)",
     ylab = "Relative activity")

lines(d21t$x, d21t$y, col = "blue", lwd = 2)
lines(d24t$x, d24t$y, col = "red",  lwd = 2)

legend("topright",
       legend = c("2018", "2021", "2024"),
       col = c("black", "blue", "red"),
       lwd = 2, bty = "n")

# Draw shaded bands for dawn and dusk

usr <- par("usr")  # xmin, xmax, ymin, ymax

rect(
  xleft = dawn_start, xright = dawn_end,
  ybottom = usr[3], ytop = usr[4],
  col = adjustcolor("grey70", alpha.f = 0.35),
  border = NA
)

rect(
  xleft = dusk_start, xright = dusk_end,
  ybottom = usr[3], ytop = usr[4],
  col = adjustcolor("grey40", alpha.f = 0.35),
  border = NA
)

usr <- par("usr")  # (xmin, xmax, ymin, ymax)
text(x = mean(c(dawn_start, dawn_end)), y = usr[4] * 0.95, labels = "Dawn", cex = 0.9)
text(x = mean(c(dusk_start, dusk_end)), y = usr[4] * 0.95, labels = "Dusk", cex = 0.9)


###########################################################
##Section I: Circular Kernel Density Model
###########################################################

# Take earliest detection per day per diel period
unique_detections <- all_detections %>%
    arrange(species1ID, date, diel_period, time_rad) %>%
    group_by(species1ID, date, diel_period) %>%
    slice(1) %>%
    ungroup()

# Create list for chi-square test
diel_tests <- list()

for (species in TARGET_SPECIES) {
    
    cat(paste0("Testing diel period shifts for ", species, "...\n"))
    
    # Get data for this species
    species_diel <- unique_detections %>%
        filter(species1ID == species) %>%
        select(Period, diel_period)
    
    if (nrow(species_diel) < 20) {
        cat("  Insufficient data\n\n")
        next
    }

# Create contingency table
diel_table <- table(species_diel$Period, species_diel$diel_period)

cat("\nContingency table:\n")
print(diel_table)
cat("\n")

# Chi-square test
chi_test <- chisq.test(diel_table)

cat(paste0("Chi-square test: X² = ", round(chi_test$statistic, 3), 
           ", df = ", chi_test$parameter, 
           ", p = ", round(chi_test$p.value, 8), "\n"))

if (chi_test$p.value < 0.05) {
    cat("*** Significant difference in diel period use across years\n")
} else {
    cat("No significant difference in diel period use across years\n")
}
)

cat("\n")
}


###########################################################
##Section J: Earliest Observed Emergence 
###########################################################

# List records of first detections of all species
first_emergence <- all_detections %>%
    filter(!is.na(time_rad), !is.na(diel_period), !is.na(date)) %>%
    mutate(date = as.Date(date)) %>%
    group_by(species1ID) %>%
    mutate(min_time_rad = min(time_rad, na.rm = TRUE)) %>%
    filter(time_rad == min_time_rad) %>%
    arrange(date) %>%                
    slice(1) %>%                     
    ungroup() %>%
    transmute(
        species1ID,
        first_time_rad = time_rad,
        first_date = date,
        first_period = Period
    )

# List first 10 species by period
First10_by_period <- first_emergence %>%
    arrange(first_period, first_date) %>%
    group_by(first_period) %>%
    slice_head(n = 10) %>%
    ungroup()

# Create timeline scatter plot for first 10 species each period
First10_by_period %>%
    mutate(
        species1ID = factor(species1ID)
    ) %>%
    ggplot(aes(x = first_date, y = species1ID)) +
    geom_point(size = 2, colour = "black") +
    facet_wrap(~ first_period, scales = "free") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")+
    labs(
        x = "First emergence date",
        y = "Species",
        title = "First 10 species detected in each survey period"
    ) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "grey90")
    )

# First emergence by taxonomy
taxon_map <- tibble::tribble(
    ~species1ID,                      ~Group,
    # 2018
    "White-throated Kingfisher",      "Avian",
    "Straw-headed Bulbul",            "Avian",
    "Slender Squirrel",               "Mammal",
    "Red-legged Crake",               "Avian",
    "Racket-tailed Drongo",           "Avian",
    "Pink-necked Green Pigeon",       "Avian",
    "Olive-backed Sunbird",           "Avian",
    "Large-tailed Nightjar",          "Avian",
    "Feral Dog",                      "Mammal",
    "Common Palm Civet",              "Mammal",
    
    # 2021
    "Wild Boar",                      "Mammal",
    "Smooth-coated otter",            "Mammal",
    "Siberian blue robin",            "Avian",
    "Red junglefowl",                 "Avian",
    "Rat",                            "Mammal",
    "Malayan water monitor",          "Other",
    "Malayan monitor lizard",         "Other",
    "Emerald dove",                   "Avian",
    "Dog",                            "Mammal",
    "Clouded monitor lizard",         "Other",
    
    # 2024
    "Sunda scops owl",                "Avian",
    "Plantain squirrel",              "Mammal",
    "Long-tailed Macaque",            "Mammal",
    "Lesser Mouse-deer",              "Mammal",
    "Greater racket-tailed drongo",   "Avian",
    "Common Treeshrew",               "Mammal",
    "Colugo",                         "Mammal",
    "Clouded monitor",                "Other",
    "Changeable hawk eagle",          "Avian",
    "Butterfly",                      "Other"
)

avian_mammal_counts <- First10_by_period %>%
left_join(taxon_map, by = "species1ID") %>%
mutate(
Group = tidyr::replace_na(Group, "Other"),
Group = factor(Group, levels = c("Avian", "Mammal", "Other"))
) %>%
count(first_period, Group, name = "n") %>%
tidyr::complete(
first_period,
Group,
fill = list(n = 0)
)

ggplot(avian_mammal_counts,
       aes(x = first_period, y = n, colour = Group)) +
    geom_point(size = 4, position = position_dodge(width = 0.1)) +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    labs(
        x = "Survey period",
        y = "Number of species in Top 10",
        title = "Avian, Mammal and Other in the top 10 first detections"
    ) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

# First emergence of target species
emergence_target <- all_detections %>%
    filter(species1ID %in% TARGET_SPECIES) %>%
    mutate(
        date = as.Date(date),
        Period = factor(Period, levels = c("2018", "2021", "2024"))
    ) %>%
    group_by(Period, species1ID) %>%
    summarise(first_date = min(date, na.rm = TRUE), .groups = "drop")

ggplot(emergence_target, aes(x = first_date, y = species1ID)) +
    geom_point(size = 3) +
    facet_wrap(~ Period, scales = "free_x") +
    labs(
        x = "First detected date",
        y = "Species",
        title = "First detection of target species by survey period"
    ) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )
