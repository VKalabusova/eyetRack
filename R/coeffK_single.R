#' Ambient/Focal Attention - Coefficient K - single
#'
#' Calculate and plot elements of Coefficient K. Coefficient K helps distinguish focal and ambient attention.
#'
#' As the attention and saccades are interconnected, it is theorised that the duration of fixations and amplitide of saccades reflects the different types of attention. In order to distinguish types of attantion, a cooefficient K has been intoduced as a metric to characterise ambient and focal attention when conduciting cartographic tasks. Ambient attention is typically characterised by relatively short fixations, which are followed by long saccades. Conversely, focal attention is defined by long fixations which are followed by short saccades. K > 0 denotes focal attention, whereas K < 0 denotes ambient attention. If our participant's attention transitions from ambient to focal, then K can indicate the end of visual searching, either triggered by boredom or making a decision according to the task's instructions.
#'
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
#' @param data data frame
#' @param eye_tracker the type of used eye-tracker ("SMI"/"Tobii"). Default value "SMI". (character)
#' @param object the name of a stimuli (character)
#' @param participant the name of a participant(character)
#' @param distance distance from screen and eyes [cm]
#' @param lim y axis scale (numeric vector)
#' @param point_col colour of points. Default value "steelblue".
#' @param point_size size of points
#' @param title_size the size of title
#' @param x_size x lab ticks font size
#' @param x_angle x lab text angle
#' @param x_labsize x lab font size
#' @param y_size y lab ticks font size
#' @param y_angle y lab text angle
#' @param y_labsize y lab font size
#'
#' @return List - data frame (K - elements of coefficient K) and plot (K_plot - visualization of elements K_i, title includes final coefficient K)
#'
#' @examples
#' coeffK_single(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16", 60, c(-5, 5))
#' coeffK_single(data_Tobii, "Tobii", "10.jpg", "Participant19", 60)
#'
#' @export
coeffK_single = function(data, eye_tracker = "SMI", object, participant, distance, lim = c(-10,10), point_col = "steelblue", point_size = 3, title_size = 13, x_size = 10, x_angle = 0, x_labsize = 10, y_size = 10, y_angle = 0, y_labsize = 10) {
  a = data
  if(eye_tracker == "Tobii") {
    data = data[data$Eye.movement.type == "Fixation",]
    data = data[data$Participant.name == participant,]
    data = data[data$Presented.Media.name == object,]
    data1 = data.frame()
    for (i in (unique(data$Eye.movement.type.index))) {
      data2 = data[data$Eye.movement.type.index == i,]
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
    data_s = data
  }
  if(!is.element(object, unlist(data$Stimulus))) {
    stop("Incorrect object.")
  }
  if(eye_tracker == "SMI"){
    if(!is.element(participant, unlist(data$Participant))) {
      stop("Incorrect Participant.")
    }
    data_s = subset(data, Stimulus == object & Participant == participant) # subset
    data_s = na.omit(data_s) # omit NA
  }
  x = data_s$Fixation.Position.X..px.
  y = data_s$Fixation.Position.Y..px.
  duration = data_s$Event.Duration..ms. # duration of fixation
  n = length(duration)
  mean_d = mean(duration)
  sd_d = sd(duration)
  euklid = function (x1, x2, y1, y2) {   # funkce na vzdalenost mezi 2 fixacemi (eukleidovska metrika)
    v = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
    return (v)
  }
  amplitude = c()
  for (j in (1:(length(duration)-1))){ # vzdalenost mezi fixacemi v px
    amplitude[j] = euklid(x[j], x[j+1], y[j], y[j+1])
  }
  amplitude_cm = amplitude*0.026458333 # zmena px na cm
  amplitude = atan(amplitude_cm/distance) # amplituda sakady stupne
  mean_a = mean(amplitude)
  sd_a = sd(amplitude)
  K = c()
  for(j in (1:(length(duration)-1))) {
    K[j] = ((duration[j] - mean_d)/sd_d) - ((amplitude[j] - mean_a)/sd_a)
  }
  K = round(K, 4)
  coeffK = data.frame(K_i = K)
  basic = list(theme_bw(), scale_y_continuous(breaks= pretty_breaks(),limits = c(lim[1], lim[2])), theme(plot.title = element_text(size=title_size, hjust = 0.5),
                                                                                                         axis.text.x = element_text(size = x_size, angle = x_angle),
                                                                                                         axis.text.y = element_text(size = y_size, angle = y_angle),
                                                                                                         axis.title.x = element_text(size = x_labsize),
                                                                                                         axis.title.y = element_text(size = y_labsize),
                                                                                                         legend.position = "none"))




  ### plot
  plot1 = ggplot(coeffK, aes(x = seq(1:length(K_i)), y = K_i)) + geom_point(col = point_col, size = point_size) + geom_hline(yintercept=0, linetype="dashed", color = "red") +
    labs(title = paste("Ambient/Focal Attention-", participant, " (",coeff_K(a, eye_tracker, object, participant, distance, c(-1,1))$K[1,2],")", sep = ""), x = "Index i", y = bquote("K"[i])) + basic

  return(list(K = coeffK, K_plot = plot1))
}
