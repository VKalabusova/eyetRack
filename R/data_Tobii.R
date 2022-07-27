#' @title Data Tobii
#'
#' @description Eye-tracking data from eye-tracker Tobii.
#'
#' @format Data frame with 15 rows and 9 variables:
#' \itemize{
#'     \item Participant.name - the name of a participant
#'     \item Presented.Media.name - the name of stimuli
#'     \item Eye.movement.type - type of eye movement (fixation)
#'     \item Gaze.event.duration - Duration of fixation
#'     \item Eye.movement.type.index - Index of fixation
#'     \item Fixation.point.X - X-position of fixation [px]
#'     \item Fixation.point.Y - Y-position of fixation [px]
#'     \item AOI.hit..10...right - hit right area of interest (1 yes, 0 no)
#'     \item AOI.hit..10...left - hit left area of interest (1 yes, 0 no)
#' }
#'
#'
"data_Tobii"
