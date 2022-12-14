% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barplot_AOI.R
\name{barplots_AOI}
\alias{barplots_AOI}
\title{Bar plots of Fixations}
\usage{
barplots_AOI(
  data,
  eye_tracker = "SMI",
  object,
  participant,
  count = "absolute",
  same_col = T,
  bar_col = "steelblue",
  bar_width = 0.7,
  top_size = 15,
  title_size = 12,
  col_man = F,
  col_set = c(),
  x_size = 10,
  x_angle = 0,
  x_labsize = 10,
  y_size = 10,
  y_angle = 0,
  y_labsize = 10
)
}
\arguments{
\item{data}{data frame}

\item{eye_tracker}{the type of used eye-tracker ("SMI"/"Tobii"). Default value "SMI". (character)}

\item{object}{the name of a stimuli (character)}

\item{participant}{the name of a participant (character)}

\item{count}{visualization of count ("absolute"/"relative"). Default value "absolute". (character)}

\item{same_col}{same colour of barplots (TRUE/FALSE). Default value TRUE.}

\item{bar_col}{colour of bars (character). Default value "steelblue".}

\item{bar_width}{width of bars}

\item{top_size}{the size of title}

\item{title_size}{the size of title of each barplot}

\item{col_man}{if you do not want to set colours of bars manually (FALSE). If you want to set colours of bars manually (TRUE). Default value is FALSE.}

\item{col_set}{vector of n colours (n - number of unique AOIs in dataset). If col_man = T.}

\item{x_size}{x lab ticks font size}

\item{x_angle}{the angle of text on x lab}

\item{x_labsize}{x lab font size}

\item{y_size}{y lab ticks font size}

\item{y_angle}{the angle of text on y lab}

\item{y_labsize}{y lab font size}
}
\value{
The left hand side barplot will represent number of fixations in each area of interest. The right hand side barplot will represent dwell time.
}
\description{
Barplot visualization allows to show a number of fixations in each Area of Interest and Dwell Time.
The data from SMI trackers must contain the following variables:
\itemize{
    \item Stimulus (Presented Media name)
    \item Participant (Participant name)
    \item Index	(Eye movement type index)
    \item Event Start Trial Time [ms]
    \item Event End Trial Time [ms]
    \item Event Duration [ms] (Gaze event duration)
    \item Fixation Position X [px]	(Fixation point X)
    \item Fixation Position Y [px] (Fixation point Y)
    \item AOI Name (AOI hit)
    \item Eye movement type - only for Tobii
}
You will be able to find the relevant variables for Tobii eye trackers within parenthesis.
}
\examples{
barplots_AOI(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16", count = "absolute")
barplots_AOI(data_Tobii, "Tobii", "10.jpg", "Participant19", count = "relative", same_col = FALSE, col_man = TRUE, col_set = c("steelblue", "yellow"))

}
