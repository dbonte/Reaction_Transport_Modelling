
RTMtutorial <- function(x = c("introduction", "why", "conceptual", "mass_balance",
    "largescale","chemical", "enzymatic", "partitioning", "ecology",
    "transport_processes","transport_fluxes", "transport_porous",
    "transport_boundary", "Rcode")) {

  LL <- as.character(formals(RTMtutorial)$x[-1])

  if (x == "?") {
    tutorial <- data.frame(x=LL, description = c("About the course at Utrecht",
      "Why modelling is useful", "Making conceptual models",
      "Creating mass balance equations", "Large-scale models (e.g. earth's C-cycle)",
      "Elementary and equilibrium chemical reactions", "Enzymatic reactions",
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
