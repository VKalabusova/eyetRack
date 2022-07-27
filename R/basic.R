#' Basic Analysis of Fixations
#'
#' It offers a basic analysis for the initial conception of the number and duration of fixations.
#' The data from SMI eye-trackers must contain the following variables:
#' \itemize{
#'     \item Stimulus (Presented Media name)
#'     \item Participant (Participant name)
#'     \item Index	(Eye movement type index)
#'     \item Event Start Trial Time [ms]
#'     \item Event End Trial Time [ms]
#'     \item Event Duration [ms] (Gaze event duration)
#'     \item Fixation Position X [px]	(Fixation point X)
#'     \item Fixation Position Y [px] (Fixation point Y)
#'     \item AOI Name (AOI hit)
#'     \item Eye movement type - only for Tobii
#' }
#' You will be able to find the relevant variables for Tobii eye trackers within parenthesis.
#'
#'
#' @param data data frame
#' @param eye_tracker the type of used eye-tracker ("SMI"/"Tobii"). Default value "SMI". (character)
#' @param object the name of a stimuli (character)
#' @param participant the name of a participant or participants (character or vector of characters)
#'
#' @return Data frame with the following variables:
#' \itemize{
#'   \item NoF - Number of Fixations
#'   \item TTFF - Time to First Fixation [ms] - NA for Tobii data
#'   \item FFD - First Fixation Duration [ms]
#'   \item TD - Total Duration [ms]
#'   \item AFD - Average Fixation Duration [ms]
#' }
#'
#' @examples
#' basic_analysis(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16")
#' basic_analysis(data_Tobii, "Tobii", "10.jpg", "Participant19")
#'
#' @importFrom dplyr rename
#'
#' @export

basic_analysis = function(data, eye_tracker = "SMI", object, participant){
  '%!in%' = function(x,y)!('%in%'(x,y))
  df_total = data.frame()
  a = data
  for (i in (1:length(participant))) {
    if(eye_tracker == "Tobii") {
      data = a[a$Eye.movement.type == "Fixation",]
      data = data[data$Participant.name == participant [i],]
      data = data[data$Presented.Media.name == object,]
      data1 = data.frame()
      for (j in (unique(data$Eye.movement.type.index))) {
        data2 = data[data$Eye.movement.type.index == j,]
        data2 = data2[1,]
        data1 = rbind(data1,data2)
      }
      data = data1
      data = rename(data, Participant = Participant.name)
      data = rename(data, Stimulus = Presented.Media.name)
      data = rename(data, Index = Eye.movement.type.index)
      data = rename(data, Event.Duration..ms. = Gaze.event.duration)
      data = rename(data, Fixation.Position.X..px. = Fixation.point.X)
      data = rename(data, Fixation.Position.Y..px. = Fixation.point.Y)
      data$Stimulus = as.factor(as.character(data$Stimulus))
      data$Participant = as.factor(as.character(data$Participant))
      data$Event.Start.Trial.Time..ms. = "NA"
      data_s = data
    }
    if(object %!in% unique(levels(data$Stimulus))){
      stop("Incorrect object.")
    }

    if (eye_tracker == "SMI") {
      if(participant[i] %!in% unique(levels(data$Participant))) {
      stop("Incorrect Participant.")
    }
    data_s = subset(data, Stimulus == object & Participant == participant[i]) # subset
    data_s = na.omit(data_s) # omit NA
    }
    duration = data_s$Event.Duration..ms. # duration of fixation
    NoF = length(duration) # Number of Fixation
    TTFF = data_s$Event.Start.Trial.Time..ms.[1] # Time to First Fixation
    FFD = duration[1] # First Fixation Duration
    TD = sum(duration) # Total Duration
    AFD = TD/NoF # Average Fixation Duration
    df = data.frame(NoF   = NoF, TTFF = TTFF, FFD = FFD, # create data.frame
                    TD = TD, AFD = AFD)
    df_total = rbind(df_total,df)
  }
  df_total = cbind("Participant" = participant, df_total) # add column with participant/s
  return(df_total)
}
