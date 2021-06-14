\name{RTMtutorial}
\alias{RTMtutorial}
\title{Reaction Transport Modelling Course tutorials}
\usage{
RTMtutorial (x = c("introduction", "why", "conceptual", "mass_balance", "largescale",
    "chemical", "enzymatic", "partitioning", "ecology", "transport_processes",
    "transport_fluxes", "transport_porous", "transport_boundary", "Rcode"))
}
\arguments{
  \item{x }{the name of the tutorial, or the number of the tutorial. When x = "?", a list of tutorials will be displayed, including a short description.
  }
}

\description{
tutorials that serve to prepare for the exam
}

\examples{
\dontrun{
 RTMtutorial("largescale")
 RTMtutorial(2)
}

RTMtutorial ("?")
}
