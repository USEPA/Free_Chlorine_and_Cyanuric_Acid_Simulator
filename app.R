#Required libraries
library(ggplot2)
library(gridExtra)
library(nleqslv)
library(plyr)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinythemes)

#SERVER FUNCTION SECTION BEGINS HERE########################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#SERVER FUNCTION GENERAL SECTION############################################################################################
############################################################################################################################

# Set parameter names for inputed conditions to pass to simulation function
sim_Names <- c("pH_range", "C_Free_mg_L_Cl2", "C_IsoCy_mg_L_low", "C_IsoCy_mg_L_high",
               "C_Andiclcy_mg_L", "C_Dihyddiclcy_mg_L", "C_Triclcy_mg_L", "Method", "Cy_range",
               "C_Andiclcy_mg_L_Cl2","C_Dihyddiclcy_mg_L_Cl2", "C_Triclcy_mg_L_Cl2")

# Define colors scheme for plots

  # log molar plot
  palette_1 <- c(
    "#000000", "#999999", "#E69F00",
    "#999999", "#E69F00",
    "#000000", "#999999", "#E69F00", "#56B4E9",
    "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00",
    "#CC79A7")

  # log mg/L plot
  palette_2 <- c(
    "#000000", "#999999",
    "#999999", "#E69F00",
    "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00",
    "#CC79A7")

  # mg/L plot
  palette_3 <- c(
    "#000000", "#999999",
    "#999999", "#E69F00")

# Define linetypes for plots

  # log molar plot
  linesets_1 <- c(
    1, 1, 1,
    2, 2,
    3, 3, 3, 3,
    2, 2, 2,
    2, 2,
    2)

  # log mg/L plot
  linesets_2 <- c(
    1, 1,
    2, 2,
    2, 2, 2,
    2, 2,
    2)

  # mg/L plot
  linesets_3 <- c(
    1, 1,
    2, 2)

# Define line sizes for plots

  # log molar plot
  linesize_1 <- c(
    1.25, 1.25, 1.25,
    1, 1,
    1, 1, 1, 1,
    1, 1, 1,
    1, 1,
    1)

  # log mg/L plot
  linesize_2 <- c(
    1.25, 1.25,
    1, 1,
    1, 1, 1,
    1, 1,
    1)

  # mg/L plot
  linesize_3 <- c(
    1.25, 1.25,
    1, 1)

# Define formatted names for plot legends

  # log molar plot
  series_1 <- c(expression(
    'TOTCl'[2]*'', 'TOTFreeCl'[2]*'', 'TOTCy',
    'HOCl', 'OCl'^'-',
    'H'[3]*'Cy', 'H'[2]*'Cy'^'-', 'HCy'^'2-', 'Cy'^'3-',
    'H'[2]*'ClCy', 'HCl'[2]*'Cy', 'Cl'[3]*'Cy',
    'HClCy'^'-', 'ClCy'^'2-',
    'Cl'[2]*'Cy'^'-')
    )

  # log mg/L plot
  series_2 <- c(expression(
    'TOTCl'[2]*'', 'TOTFreeCl'[2]*'',
    'HOCl', 'OCl'^'-',
    'H'[2]*'ClCy', 'HCl'[2]*'Cy', 'Cl'[3]*'Cy',
    'HClCy'^'-', 'ClCy'^'2-',
    'Cl'[2]*'Cy'^'-')
    )

  # mg/L plot
  series_3 <- c(expression(
    'TOTCl'[2]*'', 'TOTFreeCl'[2]*'',
    'HOCl', 'OCl'^'-')
    )

# Define basic theme used for all plots
  mytheme <-  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_line(colour = "grey85", size = 0.5),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit (2, "cm"),
    legend.key.height = unit (1.3, "line"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.25)),
    legend.position = "right",
    legend.direction = "vertical",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel (1.5)),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.line = element_line(colour = "black", size = 1, lineend = "square"),
    axis.text.x = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
    )

# Create function to copy initial conditions from one simulation to another
update_IC <- function(session, from, to, input) {

  # Update initial condition for slider inputs
  updateSliderInput(session, paste0(to, "_", "pH_range"), value = input[[paste0(from, "_", "pH_range")]])
  updateSliderInput(session, paste0(to, "_", "C_Free_mg_L_Cl2"), value = input[[paste0(from, "_", "C_Free_mg_L_Cl2")]])
  updateSliderInput(session, paste0(to, "_", "C_IsoCy_mg_L_low"), value = input[[paste0(from, "_", "C_IsoCy_mg_L_low")]])
  updateSliderInput(session, paste0(to, "_", "C_IsoCy_mg_L_high"), value = input[[paste0(from, "_", "C_IsoCy_mg_L_high")]])
  updateSliderInput(session, paste0(to, "_", "C_Andiclcy_mg_L"), value = input[[paste0(from, "_", "C_Andiclcy_mg_L")]])
  updateSliderInput(session, paste0(to, "_", "C_Andiclcy_mg_L_Cl2"), value = input[[paste0(from, "_", "C_Andiclcy_mg_L_Cl2")]])
  updateSliderInput(session, paste0(to, "_", "C_Dihyddiclcy_mg_L"), value = input[[paste0(from, "_", "C_Dihyddiclcy_mg_L")]])
  updateSliderInput(session, paste0(to, "_", "C_Dihyddiclcy_mg_L_Cl2"), value = input[[paste0(from, "_", "C_Dihyddiclcy_mg_L_Cl2")]])
  updateSliderInput(session, paste0(to, "_", "C_Triclcy_mg_L"), value = input[[paste0(from, "_", "C_Triclcy_mg_L")]])
  updateSliderInput(session, paste0(to, "_", "C_Triclcy_mg_L_Cl2"), value = input[[paste0(from, "_", "C_Triclcy_mg_L_Cl2")]])

  #Update initial conditions for select inputs
  updateSelectInput(session, paste0(to, "_", "Method"), selected = input[[paste0(from, "_", "Method")]])
  updateSelectInput(session, paste0(to, "_", "Cy_range"), selected = input[[paste0(from, "_", "Cy_range")]])

  }

# Define function to simulate chemical speciations
simulate_speciation <- function(pH_range, C_Free_mg_L_Cl2, C_IsoCy_mg_L_low, C_IsoCy_mg_L_high,
                                C_Andiclcy_mg_L, C_Dihyddiclcy_mg_L, C_Triclcy_mg_L, Method, Cy_range,
                                C_Andiclcy_mg_L_Cl2,C_Dihyddiclcy_mg_L_Cl2, C_Triclcy_mg_L_Cl2) {

  #Set Cyanuric Acid Concentration as itself range
  ifelse(Cy_range == "Cy_low_range",
         C_IsoCy_mg_L <- C_IsoCy_mg_L_low,
         C_IsoCy_mg_L <- C_IsoCy_mg_L_high)

  #Define pH values
  pH <- seq(pH_range[1], pH_range[2], by = 0.1)

  #Calculated C_H
  C_H <- 10^-pH

  #Set initial molar concentrations based on various possible input scenarios

  #Free Chlorine as chlorine (MW Eq. = 71) & Cyanuric Acid as itself (MW - 129.07)
  if (Method == "freecy") {
    C_TOTCl2 <- C_Free_mg_L_Cl2 / 71 / 1000
    C_TOTCy <- C_IsoCy_mg_L / 129.07 / 1000
  }

  #Anhydrous Sodium Dichloroisocyanurate as itself (MW = 219.95)
  if (Method == "andiclcy") {
    C_TOTCl2 <- C_Andiclcy_mg_L / 219.95 / 1000 * 2
    C_TOTCy <- C_Andiclcy_mg_L / 219.95 / 1000
  }

  #Anhydrous Sodium Dichloroisocyanurate as chlorine (MW Eq. = 71)
  if (Method == "andiclcy_Cl2") {
    C_TOTCl2 <- C_Andiclcy_mg_L_Cl2 / 71 / 1000
    C_TOTCy <- C_Andiclcy_mg_L_Cl2 / 71 / 1000 / 2
  }

  #Dihydrate Sodium Dichloroisocyanurate as itself (MW = 255.98)
  if (Method == "dihyddiclcy") {
    C_TOTCl2 <- C_Dihyddiclcy_mg_L / 255.98 / 1000 * 2
    C_TOTCy <- C_Dihyddiclcy_mg_L / 255.98 / 1000
  }

  #Dihydrate Sodium Dichloroisocyanurate as chlorine (MW Eq. = 71)
  if (Method == "dihyddiclcy_Cl2") {
    C_TOTCl2 <- C_Dihyddiclcy_mg_L_Cl2 / 71 / 1000
    C_TOTCy <- C_Dihyddiclcy_mg_L_Cl2 / 71 / 1000 / 2
  }

  #Trichloroisocyanuric Acid as itself (MW = 232.41)
  if (Method == "triclcy") {
    C_TOTCl2 <- C_Triclcy_mg_L / 232.41 / 1000 * 3
    C_TOTCy <- C_Triclcy_mg_L / 232.41 / 1000
  }

  #Trichloroisocyanuric Acid as chlorine (MW Eq. = 71)
  if (Method == "triclcy_Cl2") {
    C_TOTCl2 <- C_Triclcy_mg_L_Cl2 / 71 / 1000
    C_TOTCy <- C_Triclcy_mg_L_Cl2 / 71 / 1000 / 3
  }

  #Define required equilibrium constants
  K1a   <- 10^-1.8
  K2    <- 10^-3.75
  K4    <- 10^-5.33
  K6    <- 10^-6.88
  K7a   <- 10^-4.51
  K8    <- 10^-10.12
  K10   <- 10^-11.40
  K11a  <- 10^-6.90
  K12   <- 10^-13.5
  K     <- 10^-7.54

  #Create initial guesses
  C_HOCl_Guess_mg_L <- C_TOTCl2 * 71 * 1000 * (1 / (1 + K / C_H)) * 0.1

  #Calculate equation factors
  F_Cl3Cy   <- C_H^3 / (K12 * K11a * K8 * K7a * K2 * K1a)
  F_HCl2Cy  <- C_H^3 / (K12 * K11a * K8 * K7a * K2)
  F_H2ClCy  <- C_H^3 / (K12 * K11a * K8 * K4)
  F_Cl2Cy   <- C_H^2 / (K12 * K11a * K8 * K7a)
  F_HClCy   <- C_H^2 / (K12 * K11a * K8)
  F_ClCy    <- C_H   / (K12 * K11a)
  F_H3Cy    <- C_H^3 / (K12 * K10 * K6)
  F_H2Cy    <- C_H^2 / (K12 * K10)
  F_HCy     <- C_H   /  K12
  F_HOCl    <- 1 + K / C_H

  #Define equation for optimization
  Species_Solver <- function(C_HOCl_Guess_mg_L) {

    #Set C_HOCl to Guess
    C_HOCl <- C_HOCl_Guess_mg_L/71/1000

    #Define equation minimize to zero
    y <- C_TOTCy * ((3*F_Cl3Cy*C_HOCl^3 + 2*F_HCl2Cy*C_HOCl^2 + F_H2ClCy*C_HOCl + 2*F_Cl2Cy*C_HOCl^2 + F_HClCy*C_HOCl + F_ClCy*C_HOCl) /
                      (F_Cl3Cy*C_HOCl^3 + F_HCl2Cy*C_HOCl^2 + F_H2ClCy*C_HOCl + F_Cl2Cy*C_HOCl^2 + F_HClCy*C_HOCl + F_ClCy*C_HOCl + F_H3Cy + F_H2Cy + F_HCy + 1)) +
      C_HOCl*F_HOCl - C_TOTCl2
    y

  }

  #Solve for HOCl concentration
  result <- nleqslv(x = C_HOCl_Guess_mg_L,
                    fn = Species_Solver,
                    control = list(ftol = 1e-15, xtol = 1e-15))


  #Store mesage from equation solver
  message <- ifelse(result$termcd == 1, "Convergence - Simulation Valid", "No Convergence - Simulation Not Valid")

  #Calculate species molar concentrations
  C_HOCl    <- result$x / 71 / 1000
  C_OCl     <- K * C_HOCl / C_H
  C_Cy      <- C_TOTCy / (F_Cl3Cy*C_HOCl^3 + F_HCl2Cy*C_HOCl^2 + F_H2ClCy*C_HOCl + F_Cl2Cy*C_HOCl^2 + F_HClCy*C_HOCl + F_ClCy*C_HOCl + F_H3Cy + F_H2Cy + F_HCy + 1)
  C_Cl3Cy   <- F_Cl3Cy * C_HOCl^3 * C_Cy
  C_HCl2Cy  <- F_HCl2Cy * C_HOCl^2 * C_Cy
  C_H2ClCy  <- F_H2ClCy * C_HOCl * C_Cy
  C_Cl2Cy   <- F_Cl2Cy*C_HOCl^2 * C_Cy
  C_HClCy   <- F_HClCy*C_HOCl * C_Cy
  C_ClCy    <- F_ClCy*C_HOCl * C_Cy
  C_H3Cy    <- F_H3Cy * C_Cy
  C_H2Cy    <- F_H2Cy * C_Cy
  C_HCy     <- F_HCy * C_Cy
  C_Free    <- C_HOCl + C_OCl

  #Mass balance check on calculated concentrations
  C_TOTCl2_sim <- 3 * C_Cl3Cy + 2 * C_HCl2Cy + C_H2ClCy + 2 * C_Cl2Cy + C_HClCy + C_ClCy + C_HOCl + C_OCl
  C_TOTCy_sim  <- C_Cl3Cy + C_HCl2Cy + C_H2ClCy + C_Cl2Cy + C_HClCy + C_ClCy + C_H3Cy + C_H2Cy + C_HCy + C_Cy

  #Calculate log10 molar concentrations
  log_C_HOCl    <- log10(C_HOCl)
  log_C_OCl     <- log10(C_OCl)
  log_C_Cy      <- log10(C_Cy)
  log_C_Cl3Cy   <- log10(C_Cl3Cy)
  log_C_HCl2Cy  <- log10(C_HCl2Cy)
  log_C_H2ClCy  <- log10(C_H2ClCy)
  log_C_Cl2Cy   <- log10(C_Cl2Cy)
  log_C_HClCy   <- log10(C_HClCy)
  log_C_ClCy    <- log10(C_ClCy)
  log_C_H3Cy    <- log10(C_H3Cy)
  log_C_H2Cy    <- log10(C_H2Cy)
  log_C_HCy     <- log10(C_HCy)
  log_C_Free    <- log10(C_Free)
  log_C_TOTCl2  <- log10(C_TOTCl2_sim)
  log_C_TOTCy   <- log10(C_TOTCy_sim)

  #Calculate log10 mg Cl2/L Concentrations
  log_C_HOCl_mg_L    <- log10(C_HOCl * 71 * 1000)
  log_C_OCl_mg_L     <- log10(C_OCl * 71 * 1000)
  log_C_Cl3Cy_mg_L   <- log10(C_Cl3Cy * 3 * 71 * 1000)
  log_C_HCl2Cy_mg_L  <- log10(C_HCl2Cy * 2 * 71 * 1000)
  log_C_H2ClCy_mg_L  <- log10(C_H2ClCy * 71 * 1000)
  log_C_Cl2Cy_mg_L   <- log10(C_Cl2Cy* 2 * 71 * 1000)
  log_C_HClCy_mg_L   <- log10(C_HClCy * 71 * 1000)
  log_C_ClCy_mg_L    <- log10(C_ClCy * 71 * 1000)
  log_C_Free_mg_L    <- log10(C_Free * 71 * 1000)
  log_C_TOTCl2_mg_L  <- log10(C_TOTCl2_sim * 71 * 1000)

  #Calculate mg Cl2/L free chlorine species
  C_HOCl_mg_L    <- 10^log_C_HOCl_mg_L
  C_OCl_mg_L     <- 10^log_C_OCl_mg_L
  C_Free_mg_L    <- 10^log_C_Free_mg_L
  C_TOTCl2_mg_L  <- 10^log_C_TOTCl2_mg_L

  #Assemble data frame of log molar concentrations versus pH
  Species_DF <- data.frame(pH,
                           log_C_TOTCl2, log_C_Free, log_C_TOTCy,
                           log_C_HOCl, log_C_OCl,
                           log_C_H3Cy, log_C_H2Cy, log_C_HCy, log_C_Cy,
                           log_C_H2ClCy, log_C_HCl2Cy, log_C_Cl3Cy,
                           log_C_HClCy, log_C_ClCy,
                           log_C_Cl2Cy
  )

  #Assemble data frame of log mg Cl2/L concentrations versus pH
  Species_DF_mg_L <- data.frame(pH,
                                log_C_TOTCl2_mg_L, log_C_Free_mg_L,
                                log_C_HOCl_mg_L, log_C_OCl_mg_L,
                                log_C_H2ClCy_mg_L, log_C_HCl2Cy_mg_L, log_C_Cl3Cy_mg_L,
                                log_C_HClCy_mg_L, log_C_ClCy_mg_L,
                                log_C_Cl2Cy_mg_L
  )

  #Assemble data frame of mg Cl2/L free chlorine versus pH
  Species_DF_free <- data.frame(pH,
                                C_TOTCl2_mg_L, C_Free_mg_L,
                                C_HOCl_mg_L, C_OCl_mg_L
  )

  # Rename column names in data frames
  colnames(Species_DF) <- c("pH",
                            "Total Chlorine", "Total Free Chlorine", "Total Cyanuric Acid",
                            "HOCl", "OCl-",
                            "H3Cy", "H2Cy -", "HCy 2-", "Cy 3-",
                            "H2ClCy", "HCl2Cy", "Cl3Cy",
                            "HClCy -", "ClCy 2-",
                            "Cl2Cy -"
  )

  colnames(Species_DF_mg_L) <- c("pH",
                                 "Total Chlorine", "Total Free Chlorine",
                                 "HOCl", "OCl-",
                                 "H2ClCy", "HCl2Cy", "Cl3Cy",
                                 "HClCy -", "ClCy 2-",
                                 "Cl2Cy -"
  )

  colnames(Species_DF_free) <- c("pH",
                                 "Total Chlorine", "Total Free Chlorine",
                                 "HOCl", "OCl-"
  )

  # Restructure data frames
  Species_melt <- melt(Species_DF, id.vars= "pH", variable.name = "chemical", value.name = "log_concentration_molar")

  Species_melt_mg_L <- melt(Species_DF_mg_L, id.vars= "pH", variable.name = "chemical", value.name = "log_concentration_mg_L")

  Species_melt_free <- melt(Species_DF_free, id.vars= "pH", variable.name = "chemical", value.name = "concentration_mg_L")


  # Return list containing the two data frames
  sim <- list(Species_melt, Species_melt_mg_L, Species_melt_free, message)
  return(sim)
}

# Define function to plot simulation results in various formats
plot_sim <- function(sim, y_axis_range, y_axis_range_mg_L) {

  # Extract data sets
  Species_melt <- as.data.frame(sim[[1]])
  Species_melt_mg_L <- as.data.frame(sim[[2]])
  Species_melt_free <- as.data.frame(sim[[3]])
  message <- sim[[4]]
  total_mg_L_plot <- max(Species_melt_free[!(Species_melt_free$concentration_mg_L == Inf),3])


  # Create molar log C - pH diagram
  plot1 <- ggplot(Species_melt, aes(x = pH, y = log_concentration_molar, color = chemical, linetype = chemical, size = chemical)) +
    geom_line() +
    xlab("pH") +
    ylab("log Molar Concentration") +
    scale_x_continuous(breaks = seq(min(Species_melt[1]), max(Species_melt[1]), 1)) +
    scale_y_continuous(lim = c(max(Species_melt[!(Species_melt$log_concentration_molar == Inf),3]) - y_axis_range,
                               max(Species_melt[!(Species_melt$log_concentration_molar == Inf),3]) + 0.5
                               ),
                       breaks = seq(round(max(Species_melt[!(Species_melt$log_concentration_molar == Inf),3]) - y_axis_range),
                                    round(max(Species_melt[!(Species_melt$log_concentration_molar == Inf),3]) + 0.5),
                                    1)
                       ) +
    scale_colour_manual(labels = series_1, values = palette_1) +
    scale_linetype_manual(labels = series_1, values = linesets_1) +
    scale_size_manual(labels = series_1, values = linesize_1) +
    guides(colour = guide_legend(ncol = 1)) +
    annotate("text",
             x = min(Species_melt[1]),
             y = max(Species_melt[!(Species_melt$log_concentration_molar == Inf),3] + 0.5),
             hjust = 0,
             vjust = 0,
             size = 5,
             fontface="bold",
             label = message
             ) +
    mytheme

  plot2 <- ggplot(Species_melt_mg_L)

  plot3 <- ggplot(Species_melt_free)

  # Create mg/L log C - pH Diagram
  if(!(Species_melt_mg_L$log_concentration_mg_L[1] == -Inf)) {
    plot2 <-
      ggplot(Species_melt_mg_L, aes(x = pH, y = log_concentration_mg_L, color = chemical, linetype = chemical, size = chemical)) +
      geom_line() +
      xlab("pH") +
      ylab(expression(log~mg/L~as~Cl[2]~Concentration)) +
      scale_x_continuous(breaks = seq(min(Species_melt_mg_L[1]), max(Species_melt_mg_L[1]), 1)) +
      scale_y_continuous(lim = c(max(Species_melt_mg_L[!(Species_melt_mg_L$log_concentration_mg_L == Inf),3]) - y_axis_range,
                                 max(Species_melt_mg_L[!(Species_melt_mg_L$log_concentration_mg_L == Inf),3])
                                 ),
                         breaks = seq(round(max(Species_melt_mg_L[!(Species_melt_mg_L$log_concentration_mg_L == Inf),3]) - y_axis_range),
                                      round(max(Species_melt_mg_L[!(Species_melt_mg_L$log_concentration_mg_L == Inf),3])),
                                      1)
                         ) +
      scale_colour_manual(labels = series_2, values = palette_2) +
      scale_linetype_manual(labels = series_2, values = linesets_2) +
      scale_size_manual(labels = series_2, values = linesize_2) +
      guides(colour = guide_legend(ncol = 1)) +
      mytheme

  #Create free chlorine plot
  plot3 <- ggplot(Species_melt_free, aes(x = pH, y = concentration_mg_L, color = chemical, linetype = chemical, size = chemical)) +
    geom_line() +
    xlab("pH") +
    ylab(expression(mg/L~as~Cl[2]~Concentration)) +
    scale_x_continuous(breaks = seq(min(Species_melt_free[1]), max(Species_melt_free[1]), 1)) +
    scale_y_continuous(lim = c(total_mg_L_plot * y_axis_range_mg_L[1] / 100,
                               total_mg_L_plot * y_axis_range_mg_L[2] / 100
                               )
                       ) +
    scale_colour_manual(labels = series_2, values = palette_2) +
    scale_linetype_manual(labels = series_2, values = linesets_2) +
    scale_size_manual(labels = series_2, values = linesize_2) +
    guides(colour = guide_legend(ncol = 1)) +
    mytheme
  }

  # Return plots
  plot <- grid.arrange(plot1, plot2, plot3, ncol=1)

  return(plot)
}

#SERVER FUNCTION DEFINITION SECTION#########################################################################################
############################################################################################################################

# Define server logic required to run simulations and produce output
server <- function(input, output, session) {

  #Copy Simulation A's inputs to Simulation B's inputs
  observe({
    #Take a dependency on input$AtoBIC
    if(input$A_to_B_IC == 0) return(NULL)

    isolate(update_IC(session, "A", "B", input))
  })

  #Copy Simulation B's inputs to Simulation A's inputs
  observe({
    #Take a dependency on input$BtoAIC
    if(input$B_to_A_IC == 0) return(NULL)

    isolate(update_IC(session, "B", "A", input))
  })

  # Define function to get inputted initial conditions and states based on prefix provided
  sim_Params <- function(prefix) {
    params <- lapply(sim_Names, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
  }

  # Run simulation based on provided initial conditions
  simA <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateA == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_speciation, sim_Params("A")))
  })

  simB <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateB == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_speciation, sim_Params("B")))
  })

  # Produce desired reactive plots
  output$A <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateA == 0) return(NULL)
    input$plotupdateA

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simA(),input$A_y_axis_range, input$A_y_axis_range_mg_L))
  })

  output$B <- renderPlot({
    #Take a dependency on input$plotupdate and input$simupdate
    if(input$simupdateB == 0) return(NULL)
    input$plotupdateB

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    isolate(plot_sim(simB(),input$B_y_axis_range, input$B_y_axis_range_mg_L))
  })

  # Expression that gets data to be downloaded at User's request
  output$A_downloadData <- downloadHandler(
    filename = function() {
      paste('A', '_', substr(as.character(Sys.time()), 1, 10),
            '_', substr(as.character(Sys.time()), 12, 13),
            '_', substr(as.character(Sys.time()), 15, 16),
            '.csv', sep = '')
    },
    content = function(file) {write.csv(simA()[[1]], file, row.names = TRUE)
    }
  )

  output$B_downloadData <- downloadHandler(
    filename = function() {
      paste('B', '_', substr(as.character(Sys.time()), 1, 10),
            '_', substr(as.character(Sys.time()), 12, 13),
            '_', substr(as.character(Sys.time()), 15, 16),
            '.csv', sep = '')
    },
    content = function(file) {write.csv(simB()[[1]], file, row.names = TRUE)
    }
  )
}

#UI OBJECT SECTION BEGINS HERE##############################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#UI OBJECT GENERAL SECTION##################################################################################################
############################################################################################################################

# Define function to take inputs for initial conditions for simulations
render_inputs <- function(prefix, prefix2) {
  wellPanel(
    h3(paste0("Simulation ", prefix, " Inputs")),

    br(),

    # Call to display initial simulation notification
    conditionalPanel(condition = paste0("input.simupdate", prefix, "== 0"),
                     tags$div("Note: An initial simulation has not been run; therefore, no plot has been generated", id = "initialsim")
    ),
    br(),
    h4("Conditions for Chemical Speciation Plots"),

    # Enter conditions for speciation calculations

    # Create input and tooltip pH range of simulation
    sliderInput(paste0(prefix, "_", "pH_range"),
                label = p("pH Range", style = "font-size: 12px"),
                min = 0.0,
                max = 14.0,
                value = c(6.0,11.0),
                step = 0.5),
    br(),
    bsTooltip(id = paste0(prefix, "_", "pH_range"),
              "Set slider to desired pH simulation range. If an error occurs when running a simulation (i.e., plots are not generated), expand the pH range and run the simulation again.",
              "top",
              options = list(container = "body")),

  #Create input and tooltip for method of chemical addition
  selectInput(paste0(prefix, "_", "Method"),
              label = h4("Chemical Addition Scenarios"),
              choices = c("Free Chlorine as chlorine (71 Eq. MW) & Cyanuric Acid as itself (129.07 MW)" = "freecy",
                          "Anhydrous Sodium Dichloroisocyanurate as itself (219.95 MW)" = "andiclcy",
                          "Anhydrous Sodium Dichloroisocyanurate as chlorine (71 Eq. MW)" = "andiclcy_Cl2",
                          "Dihydrate Sodium Dichloroisocyanurate as itself (255.98 MW)" = "dihyddiclcy",
                          "Dihydrate Sodium Dichloroisocyanurate as chlorine (71 Eq. MW)" = "dihyddiclcy_Cl2",
                          "Trichloroisocyanuric Acid as itself (232.41 MW)" = "triclcy",
                          "Trichloroisocyanuric Acid as chlorine (71 Eq. MW)" = "triclcy_Cl2"),
              selected = "freecy",
              selectize = TRUE),
  bsTooltip(id = paste0(prefix, "_", "Method"),
            paste0("Select which chemicals are being added. Refer to the Application Documentation for further information on the chemical addition scenarios."),
            "top",
            options = list(container = "body")),

  br(),

  #Only show panel if Free Chlorine & Cyanuric Acid is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'freecy'"),

    # Create input and tooltip for known total chlorine concentration
    sliderInput(paste0(prefix, "_", "C_Free_mg_L_Cl2"),
                label = p(HTML("Added Free Chlorine Concentration (mg Cl<sub>2</sub>/L)"),
                          style = "font-size: 12px"),
                min = 0.00,
                max = 10.00,
                value = 2.50,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Free_mg_L_Cl2"),
              "Set slider to known added free chlorine concentration in mg per liter as chlorine. If this is a drinking water sample, the free chlorine DPD measurement should be entered as this represents the total chlorine in the drinking water sample because of known free chlorine DPD method interferences (see Application Documentation).",
              "top",
              options = list(container = "body")),

    br(),

    #Create input and tooltip for cyanuric acid range selection
    selectInput(paste0(prefix, "_", "Cy_range"),
                label = p("Desired Cyanuric Acid Concentration Input Range", style = "font-size: 12px"),
                choices = c("Low Concentration Range (0 to 10 mg/L as itself)" = "Cy_low_range",
                            "High Concentration Range (0 to 100 mg/L as itself)" = "Cy_high_range"),
                selected = "Cy_low_range",
                selectize = TRUE),
    bsTooltip(id = paste0(prefix, "_", "Cy_range"),
              paste0("Select the numeric range for entering cyanuric acid concentrations"),
              "top",
              options = list(container = "body")),

    br(),

    conditionalPanel(
      condition = paste0("input.", prefix, "_", "Cy_range == 'Cy_low_range'"),

      # Create input and tooltip for low range known total cyanuric acid concentration
      sliderInput(paste0(prefix, "_", "C_IsoCy_mg_L_low"),
                  label = p("Added Cyanuric Acid Concentration (mg/L as itself)",
                            style = "font-size: 12px"),
                  min = 0.00,
                  max = 10.00,
                  value = 1.00,
                  step = 0.01),
      bsTooltip(id = paste0(prefix, "_", "C_IsoCy_mg_L_low"),
                "Set slider to known added cyanuric acid concentration in mg per liter as itself.  If this is a drinking water sample, enter the calculated total cyanuric acid concentration from the original dosage of cyanurate containing chemicals (see Application Documentation).",
                "top",
                options = list(container = "body"))
      ),

    conditionalPanel(
      condition = paste0("input.", prefix, "_", "Cy_range == 'Cy_high_range'"),

      # Create input and tooltip for high range known total Cyanuric acid concentration
      sliderInput(paste0(prefix, "_", "C_IsoCy_mg_L_high"),
                  label = p("Added Cyanuric Acid Concentration (mg/L as itself)",
                            style = "font-size: 12px"),
                  min = 0.0,
                  max = 100.,
                  value = 10.0,
                  step = 0.1),
      bsTooltip(id = paste0(prefix, "_", "C_IsoCy_mg_L_high"),
                "Set slider to known added cyanuric acid concentration in mg per liter as itself.  If this is a drinking water sample, enter the calculated total cyanuric acid concentration from the original dosage of cyanurate containing chemicals (see Application Documentation).",
                "top",
                options = list(container = "body"))
      )
    ),

  #Only show panel if Anhydrous Sodium Dichloroisocyanurate as itself is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'andiclcy'"),

    # Create input and tooltip for known anhydrous sodium dichloroisocyanurate concentration as itself
    sliderInput(paste0(prefix, "_", "C_Andiclcy_mg_L"),
                label = p("Added Anhydrous Sodium Dichloroisocyanurate Concentration (mg/L as itself)",
                          style = "font-size: 12px"),
                min = 0.01,
                max = 10.00,
                value = 1.00,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Andiclcy_mg_L"),
              "Set slider to known added anhydrous sodium dichloroisocyanurate in mg per liter as itself",
              "top",
              options = list(container = "body")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    ),

  #Only show panel if Anhydrous Sodium Dichloroisocyanurate as chlorine is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'andiclcy_Cl2'"),

    # Create input and tooltip for known anhydrous sodium dichloroisocyanurate concentration as chlorine
    sliderInput(paste0(prefix, "_", "C_Andiclcy_mg_L_Cl2"),
                label = p(HTML("Added Anhydrous Sodium Dichloroisocyanurate Concentration (mg Cl<sub>2</sub>/L)"),
                          style = "font-size: 12px"),
                min = 0.01,
                max = 10.00,
                value = 1.00,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Andiclcy_mg_L_Cl2"),
              "Set slider to known added anhydrous sodium dichloroisocyanurate in mg per liter as chlorine",
              "top",
              options = list(container = "body")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  ),

  #Only show panel if Dihydrate Sodium Dichloroisocyanurate as itself is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'dihyddiclcy'"),

    # Create input and tooltip for known dihydrate sodium dichloroisocyanurate concentration as itself
    sliderInput(paste0(prefix, "_", "C_Dihyddiclcy_mg_L"),
                label = p("Added Dihydrate Sodium Dichloroisocyanurate Concentration (mg/L as itself)",
                          style = "font-size: 12px"),
                min = 0.01,
                max = 10.00,
                value = 1.00,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Dihyddiclcy_mg_L"),
              "Set slider to known added dihydrate sodium dichloroisocyanurate in mg per liter as itself",
              "top",
              options = list(container = "body")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    ),

  #Only show panel if Dihydrate Sodium Dichloroisocyanurate as chlorine is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'dihyddiclcy_Cl2'"),

    # Create input and tooltip for known dihydrate sodium dichloroisocyanurate concentration as chlorine
    sliderInput(paste0(prefix, "_", "C_Dihyddiclcy_mg_L_Cl2"),
                label = p(HTML("Added Dihydrate Sodium Dichloroisocyanurate Concentration (mg Cl<sub>2</sub>/L)"),
                          style = "font-size: 12px"),
                min = 0.01,
                max = 10.00,
                value = 1.00,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Dihyddiclcy_mg_L_Cl2"),
              "Set slider to known added dihydrate sodium dichloroisocyanurate in mg per liter as chlorine",
              "top",
              options = list(container = "body")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    ),

  #Only show panel if Trichloroisocyanuric Acid as itself is selected
  conditionalPanel(
    condition = paste0("input.", prefix, "_", "Method == 'triclcy'"),

    # Create input and tooltip for known Trichloroisocyanuric Acid concentration as itself
    sliderInput(paste0(prefix, "_", "C_Triclcy_mg_L"),
                label = p("Added Trichloroisocyanuric Acid Concentration (mg/L as itself)",
                          style = "font-size: 12px"),
                min = 0.01,
                max = 10.00,
                value = 1.00,
                step = 0.01),
    bsTooltip(id = paste0(prefix, "_", "C_Triclcy_mg_L"),
              "Set slider to known added trichloroisocyanuric acid in mg per liter as itself",
              "top",
              options = list(container = "body")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    ),

    #Only show panel if Trichloroisocyanuric Acid as chlorine is selected
    conditionalPanel(
      condition = paste0("input.", prefix, "_", "Method == 'triclcy_Cl2'"),

      # Create input and tooltip for known Trichloroisocyanuric Acid concentration as chlorine
      sliderInput(paste0(prefix, "_", "C_Triclcy_mg_L_Cl2"),
                  label = p(HTML("Added Trichloroisocyanuric Acid Concentration (mg Cl<sub>2</sub>/L)"),
                            style = "font-size: 12px"),
                  min = 0.01,
                  max = 10.00,
                  value = 1.00,
                  step = 0.01),
      bsTooltip(id = paste0(prefix, "_", "C_Triclcy_mg_L_Cl2"),
                "Set slider to known added trichloroisocyanuric acid in mg per liter as chlorine",
                "top",
                options = list(container = "body")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
      ),

  br(),
  br(),

  # Copy inputs from one simulation to the other
  actionButton(paste0(prefix, "_to_", prefix2, "_IC"),
               paste0("Copy Simulation ", prefix, "'s Inputs to Simulation ", prefix2, "'s Inputs"),
               icon("copy")
  ),
  bsTooltip(id = paste0(prefix, "_to_", prefix2, "_IC"),
            "Press button to copy the current simulation inputs to the other simulation",
            "top",
            options = list(container = "body")),
  br(),
  br(),

  # Update simulation
  actionButton(paste0("simupdate", prefix),
               paste0("Update Simulation ", prefix, " (Press after Finished Changing Simulation Inputs)"), icon("refresh")),
  bsTooltip(id = paste0("simupdate", prefix),
            "Press button to update simulation using current input settings",
            "top",
            options = list(container = "body")),
  br(),
  br(),

  downloadButton(paste0(prefix, "_", "downloadData"),
                 paste0("Simulation ", prefix, " ", "Chemical Concentration Data Download (.csv file)")),
  bsTooltip(id = paste0(prefix, "_", "downloadData"),
            "Press button to download Log C - pH plot data to a comma seperated variable (.csv) file for use in another program (e.g., Excel). Only download data if plots have been generated.",
            "top",
            options = list(container = "body"))
    )
}

#Define function to take inputs for plot generation
render_plot_inputs <- function(prefix) {
  wellPanel(
    fluidRow(
      #Section title
      h4(paste0("Simulation ", prefix, " Plot Preferences")),

      br(),
      br(),

      #Create input and tooltip to select options for log concentration output plots
      sliderInput(paste0(prefix, "_", "y_axis_range"),
                  label = p("Select Range for Y-axis on log Concentration Plots", style = "font-size: 12px"),
                  min = 1,
                  max = 20,
                  value = 10,
                  step = 0.5),

      bsTooltip(id = paste0(prefix, "_", "y_axis_range"),
                "Select number of log units to display on y-axis in log output plots (i.e., the top two plots)",
                "top",
                options = list(container = "body")),

      br(),
      br(),

      # Create input and tooltip to selection option for mg/L plot
      sliderInput(paste0(prefix, "_", "y_axis_range_mg_L"),
                  label = p("Select the Percent of Total Chlorine to use for the Lower and Upper Limits on the mg/L Plot Y-axis", style = "font-size: 12px"),
                  min = 0,
                  max = 100,
                  value = c(0,100),
                  step = 5,
                  post = "%"),
      br(),
      bsTooltip(id = paste0(prefix, "_", "y_axis_range_mg_L"),
                "Set lower and upper slider values to the desired percent of total chlorine range to display on the mg/L plot (i.e., the bottom plot)",
                "top",
                options = list(container = "body")),

      #Create button and tooltip to update plots
      actionButton(paste0("plotupdate", prefix),
                   paste0("Update Plots for Simulation ", prefix),
                   icon("refresh")),
      bsTooltip(id = paste0("plotupdate", prefix),
                "Press button to update output plots with current plot settings (does not rerun simulation)",
                "top",
                options = list(container = "body"))
      )
  )
  }

# Define function to take render speciation plots
render_plot_outputs <- function(prefix) {
  wellPanel(
    h3(paste0("Simulation ", prefix, " Chemical Speciation Plots")),
    br(),
    plotOutput(prefix, height = "1000px")
  )
  }

#UI OBJECT DEFINITION SECTION###############################################################################################
############################################################################################################################

#Define UI layout
ui <- shinyUI(fluidPage(theme = shinytheme("flatly"),

  #Define header
  tags$head(tags$style(type = "text/css",

  #Define progress bar class
    "#loadmessage {
        position: fixed;
        width: 50%;
        top: 25%;
        right: 25%;
        text-align: center;
        font-weight: bold;
        font-size: 300%;
        color: black;
        padding: 10px;
        word-wrap: break-word;
        line-height: 40px;
        border-style: solid;
        border-width: large;
        border-color: black;
        border-radius: 15px;
        background-color: #f5f5f5;
        opacity: 1;
        z-index: 105;}",

  #Define style for buttons
  ".btn {
      width: 100%;
      word-wrap: break-word;
      white-space: normal;}",

  #Define initial simulation not run warning style
  "#initialsim {
       width: 90%;
       top: 0%;
       left: 5%;
       text-align: center;
       font-weight: bold;
       font-size: 12px;
       color: black;
       padding: 7.5px;
       word-wrap: break-word;
       line-height: 15px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: Yellow;
       opacity: 1;
       z-index: 105;}"
  )
  ),

####Added from EPA template####################################################################################################################################
tags$body(class = "html wide-template"),
tags$head(tags$link(rel = "stylesheet",
                    type = "text/css", href = "style.css")),

  # Header
  HTML("<header class='masthead clearfix' role='banner'>
       <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov/' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'>"),

  # Search Form
  #$form(action='https://search.epa.gov/epasearch/epasearch', class='epa-search', method='get',
  #      tags$label(class='element-hidden'),
  #      tags$input(autocomplete='off', class='form-text ui-autocomplete-input', id='search-box', name='querytext', placeholder='Search EPA.gov', value=''),
  #  tags$span( class='ui-helper-hidden-accessible', role='status'),
  #  tags$button(class='epa-search-button', id='search-button', title='Search', type='submit'),
  #  tags$input(name='areaname', type='hidden', value=''),
  #  tags$input(name='areacontacts', type='hidden', value=''),
  #  tags$input(name='areasearchurl', type='hidden', value=''),
  #  tags$input(name='typeofsearch', type='hidden', value='epa'),
  #  tags$input(name='result_template', type='hidden', value='2col.ftl')
  #),

  HTML("</div>
       </div>
       </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <span class='related-info'><strong>Related Topics:</strong></span>
       <ul class='menu pipeline'>
       <li class='menu-item'><a href='https://www.epa.gov/environmental-topics'>Environmental Topics</a></li>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/home/forms/contact-epa'>Contact Us</a></li>
       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
       <h1  class='page-title'>Free Chlorine and Cyanuric Acid System Simulator</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>"),
####Added from EPA template####################################################################################################################################

  #Call to display progress bar
  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                   tags$div("Update in progress...", id = "loadmessage")
  ),

  #Application title block
  h4("Version 0.50, Last Updated May 10, 2017"),

  h4("Created by David G. Wahman (wahman.david@epa.gov), United States Environmental Protection Agency"),

  p("The provided application simulates the water chemistry, at the selected conditions, associated with the free chlorine and cyanuric acid system (i.e., chlorinated cyanurates).
    The application allows the user to estimate the free chlorine concentration when cyanuric acid is present as is the case when adding chlorine-containing chemicals
    commonly referred to as Dichlor (anhydrous sodium dichloroisocyanurate or sodium dichloroisocyanurate dihydrate) or Trichlor (trichloroisocyanuric acid) to water.
    Equilibrium equations and associated constants are for a temperature of 25 degrees Celsius as presented by Obrien et al.",

    a(target = "_blank", href="http://www.troublefreepool.com/~richardfalk/pool/OBrien.pdf", "(Chemistry of Water Supply, Treatment, and Distribution,
      1974, pp 333-358).")
    ),

  p("To open a document describing the application in a new window, click on the following link: ",

    a(target = "_blank", href = "manual.pdf", "Application Documentation")

    ),

  p("The application was developed by the United States Environmental Protection Agency (EPA).
    No warranty expressed or implied is made regarding the accuracy
    or utility of the system, nor shall the act of distribution constitute any such warranty.  EPA has relinquished
    control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability
    of the information.  Any reference to specific commercial products, processes, or services by service mark,
    trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by
    EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity
    by EPA or the United States Government.  The views expressed in this application do not necessarily represent the views
    or policies of the EPA. Although a reasonable effort has been made to assure that the results obtained are correct,
    this application is experimental.  Therefore, the author and the EPA are not responsible and assume no liability whatsoever
    for any results or any use made of the results obtained from this application, nor for any damages or litigation that result
    from the use of the application for any purpose."),

  hr(),

  #Layout for initial conditions and plots
  fluidRow(
    column(6,
           render_inputs("A", "B"),
           hr(),
           render_plot_inputs("A"),
           render_plot_outputs("A")
           ),
    column(6,
           render_inputs("B", "A"),
           hr(),
           render_plot_inputs("B"),
           render_plot_outputs("B")
           )
    ),

####Additional required contact section########################################################################################################################
hr(),
p( a(href="https://www.epa.gov/home/forms/contact-epa", "Contact Us"),
   " to ask a question, provide feedback, or report a problem."),

####Added from EPA template####################################################################################################################################
# Footer
HTML("</div>
     </div>
     </div>
     </div>
     </section>
     <footer class='main-footer clearfix' role='contentinfo'>
     <div class='main-footer__inner'>
     <div class='region-footer'>
     <div class='block-pane-epa-global-footer' id='block-pane-epa-global-footer'>
     <div class='row cols-3'>
     <div class='col size-1of3'>
     <div class='col__title'>
     Discover.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/accessibility'>Accessibility</a></li>
     <li><a href='https://www.epa.gov/aboutepa/administrator-gina-mccarthy'>EPA Administrator</a></li>
     <li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
     <li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
     <li><a href='https://www.epa.gov/home/grants-and-other-funding-opportunities'>Grants</a></li>
     <li><a href='https://19january2017snapshot.epa.gov'>January 19, 2017 Web Snapshot</a></li>
     <li><a href='https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees'>No FEAR Act Data</a></li>
     <li><a href='https://www.epa.gov/privacy'>Privacy</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Connect.
     </div>
     <ul class='menu'>
     <li><a href='https://www.data.gov/'>Data.gov</a></li>
     <li><a href='https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general'>Inspector General</a></li>
     <li><a href='https://www.epa.gov/careers'>Jobs</a></li>
     <li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
     <li><a href='https://www.epa.gov/open'>Open Government</a></li>
     <li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
     <li><a href='https://www.epa.gov/newsroom/email-subscriptions'>Subscribe</a></li>
     <li><a href='https://www.usa.gov/'>USA.gov</a></li>
     <li><a href='https://www.whitehouse.gov/'>White House</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Ask.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/home/forms/contact-us'>Contact Us</a></li>
     <li><a href='https://www.epa.gov/home/epa-hotlines'>Hotlines</a></li>
     <li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
     <li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
     </ul>
     <div class='col__title'>
     Follow.
     </div>
     <ul class='social-menu'>
     <li><a class='menu-link social-facebook' href='https://www.facebook.com/EPA'>Facebook</a></li>
     <li><a class='menu-link social-twitter' href='https://twitter.com/epa'>Twitter</a></li>
     <li><a class='menu-link social-youtube' href='https://www.youtube.com/user/USEPAgov'>YouTube</a></li>
     <li><a class='menu-link social-flickr' href='https://www.flickr.com/photos/usepagov'>Flickr</a></li>
     <li><a class='menu-link social-instagram' href='https://www.instagram.com/epagov'>Instagram</a></li>
     </ul>
     <p class='last-updated'>Last updated on January 31, 2019</p>
     </div>
     </div>
     </div>
     </div>
     </div>
     </footer>")
####Added from EPA template####################################################################################################################################

)
)

#APPLICATION FUNCTION CALL DEFINITION#######################################################################################
############################################################################################################################
shinyApp(ui = ui, server = server)