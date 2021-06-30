

RTMtutorial <- function(x = c("introduction", "why", "conceptual", "massbalance",
    "largescale","chemical", "equilibrium", "enzymatic", "partitioning", "ecology",
    "transportProcesses","transportFluxes", "transportPorous",
    "transportBoundaries", "Rcode")) {

  LL <- as.character(formals(RTMtutorial)$x[-1])

  if (x == "?") {
    tutorial <- data.frame(x=LL, description = c("About the course at Utrecht",
      "Why modelling is useful", "Making conceptual models",
      "Creating mass balance equations", "Large-scale models (e.g. earth's C-cycle)",
      "Elementary chemical reactions", 
      "Equilibrium (reversible) chemical reactions", 
      "Enzymatic reactions",
      "Chemical reactions partitioning between phases", "Ecological reactions",
      "The general transport equation", "Advection and diffusion/dispersion",
      "Reaction tranport in porous media", "Boundary conditions in transport models",
      "Modelling in the R language"))
    return(tutorial)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), LL)]
   else
     Which <- LL[x]
   if (length(Which) > 1)
     for (w in Which)
      run_tutorial(w, package = "RTM")
   else
      run_tutorial(Which, package = "RTM")
  }
}

openRmdFile <- function(file, type) {
  
  if (type == "RMD")
     browseURL(file)
  else if (type == "PDF")
     browseURL(rmarkdown::render(input = file, output_format = "pdf_document",
                       output_file=tempfile(fileext = ".pdf")))
  else if (type == "HTML")
     browseURL(rmarkdown::render(input = file, output_format = "html_document",
                       output_file=tempfile(fileext = ".html")))
  else if (type == "WORD")
     browseURL(rmarkdown::render(input = file, output_format = "word_document",
                       output_file=tempfile(fileext = ".doc")))
}


RTMexercise <- function(x = c("modelersR", "conceptual", "massbalance",
    "carbonCycle", "ozone", "dissolutionSi", "equilibriumNH3", "equilibriumHCO3",
    "equilibriumOMD", "detritus", "COVID", "npzd", "crops_weeds", 
    "riverAnoxia", "Pdiagenesis"), 
    type = c("HTML", "PDF", "RMD", "WORD")) {

  LL <- as.character(formals(RTMexercise)$x[-1])
  type <- match.arg(toupper(type), choices=c("HTML", "PDF", "RMD", "WORD"))

  if (x == "?") {
    exercise <- data.frame(x=LL, description = c("Learning R for modellers",
      "Translating problems into a conceptual scheme", 
      "Creating mass balance equations", 
      "An earth-system box model of the C-cycle",
      "Ozone dynamics in the troposphere (elementary chemical reaction)", 
      "Dissolution kinetics of silica particles (partitioning reaction)",
      "Equilibrium chemistry - ammonium/ammonia", 
      "Equilibrium chemistry - the carbonate system", 
      "Mineralisation impact on pH (mixed equilibrium / biogeochemical reactions)", 
      "Bacterial decay of detritus (biogeochemistry)",
      "The COVID pandemic (population dynamics)", 
      "NPZD model (marine ecosystem model)",
      "Crops and weed competition (agricultural model) including economics",
      "Anoxia in a river (1-D reaction transport model)",    
      "Simple phosphorus diagenesis in marine sediment (porous medium)"
      ))
    return(exercise)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), tolower(LL))]
   else
     Which <- LL[x]
   Which <- paste(Which, "/", Which,"_Q", sep="")

  # The files are stored in RMD format 
   RMD <- paste0(system.file('exercises', package = 'RTM'),"/",Which, ".Rmd", sep="")
   
   if (length(RMD) > 1) {
     for (file in RMD) openRmdFile(file, type)
   } else openRmdFile(RMD, type)
  }   
}

# A private function - to be used as RTM:::RTManswer
RTManswer <- function(x = c("modelersR", "conceptual", "massbalance",
    "carbonCycle", "ozone", "dissolutionSi", "equilibriumNH3", "equilibriumHCO3",
    "equilibriumOMD", "detritus", "COVID", "npzd", "crops_weed", 
    "riverAnoxia", "Pdiagenesis"), 
    type = c("HTML", "PDF", "RMD", "WORD")) {

  LL <- as.character(formals(RTMexercise)$x[-1])
  type <- match.arg(toupper(type), choices=c("HTML", "PDF", "RMD", "WORD"))

  if (x == "?") {
    exercise <- data.frame(x=LL, description = c("Learning R for modellers",
      "Translating problems into a conceptual scheme", 
      "Creating mass balance equations", 
      "An earth-system box model of the C-cycle",
      "Ozone dynamics in the troposphere (elementary chemical reaction)", 
      "Dissolution kinetics of silica particles (partitioning reaction)",
      "Equilibrium chemistry - ammonium/ammonia", 
      "Equilibrium chemistry - the carbonate system", 
      "Mineralisation impact on pH (mixed equilibrium / biogeochemical reactions)", 
      "Bacterial decay of detritus (biogeochemistry)",
      "The COVID pandemic (population dynamics)", 
      "NPZD model (marine ecosystem model)",
      "Crops and weed competition (agricultural model) including economics",
      "Anoxia in a river (1-D reaction transport model)",    
      "Simple phosphorus diagenesis in marine sediment (porous medium)"
      ))
    return(exercise)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), tolower(LL))]
   else
     Which <- LL[x]
   Which <- paste(Which, "/", Which, sep="")  # THIS DIFFERS FROM RTMexercise

  # The files are stored in RMD format 
   RMD <- paste0(system.file('exercises', package = 'RTM'),"/",Which, ".Rmd", sep="")
   
   if (length(RMD) > 1) {
     for (file in RMD) openRmdFile(file, type)
   } else openRmdFile(RMD, type)
  }   
}

RTMreader <- function(x = c("events", "forcings", "observations",
    "fitting", "multidimensional_visualisation", "pHprofiles", 
    "perturbation_theory_I", "perturbation_theory_II",
    "interactive", "numericalR", "git_sharing_code"), 
    type = c("HTML", "PDF", "RMD", "WORD")) {

  LL <- as.character(formals(RTMreader)$x[-1])
  type <- match.arg(toupper(type), choices=c("HTML", "PDF", "RMD", "WORD"))
  
  if (x == "?") {
    goodies <- data.frame(x=LL, description = c("Events in dynamic models developed in R",
      "Forcing functions based on data in models developed in R", 
      "Showing observed data alongside model results in R", 
      "Fitting a 1D reaction-transport model to data in R using the FME package",
      "Visualising dynamic outputs from a 1D reaction-transport model in R", 
      "Estimating pH in a 1D reaction-transport model in R",
      "Response of systems to a perturbation from equilibrium --- Part I", 
      "Response of systems to a perturbation from equilibrium --- Part II", 
      "Interactive applications in R", 
      "Numerical methods used for reaction-transport modelling in R", 
      "Git, GitLab/Github and RStudio - sharing code with the world"
      ))
    return(goodies)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), tolower(LL))]
   else
     Which <- LL[x]
   Which <- paste("_", Which, sep="")
   
  # The files are stored in RMD format 
   RMD <- paste0(system.file('readers', package = 'RTM'),"/",Which, ".Rmd", sep="")
   
   if (length(RMD) > 1) {
     for (file in RMD) openRmdFile(file, type)
   } else openRmdFile(RMD, type)
  }   
}
