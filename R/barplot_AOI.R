#' Bar plots of Fixations
#'
#' Barplot visualization allows to show a number of fixations in each Area of Interest and Dwell Time.
#' The data from SMI trackers must contain the following variables:
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
#' @param participant the name of a participant (character)
#' @param count visualization of count ("absolute"/"relative"). Default value "absolute". (character)
#' @param same_col same colour of barplots (TRUE/FALSE). Default value TRUE.
#' @param bar_col colour of bars (character). Default value "steelblue".
#' @param bar_width width of bars
#' @param title_size the size of title
#' @param col_man if you do not want to set colours of bars manually (FALSE). If you want to set colours of bars manually (TRUE). Default value is FALSE.
#' @param col_set vector of n colours (n - number of unique AOIs in dataset). If col_man = T.
#' @param x_size the size of text ticks on x lab
#' @param x_angle the angle of text on x lab
#' @param x_labsize the size of  text on x lab
#' @param y_size the size of text ticks on y lab
#' @param y_angle the angle of text on y lab
#' @param y_labsize the size of text on y lab
#'
#' @import ggplot2
#' @importFrom ggpubr text_grob
#' @import gridExtra
#' @import patchwork
#' @import scales
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @return The left hand side barplot will represent number of fixations in each area of interest. The right hand side barplot will represent dwell time.
#'
#' @examples barplots_AOI(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16", count = "absolute")
#' @examples barplots_AOI(data_Tobii, "Tobii", "10.jpg", "Participant19", count = "relative", same_col = F, col_man = T, col_set = c("steelblue", "yellow"))
#'
#' @export
barplots_AOI = function (data, eye_tracker = "SMI", object, participant, count = "absolute", same_col = T, bar_col = "steelblue", bar_width = 0.7, top_size = 15, title_size = 12, col_man = F, col_set = c(), x_size = 10, x_angle = 0, x_labsize = 10, y_size = 10, y_angle = 0, y_labsize = 10){
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
    data1 = cbind(data, AOI.Name = NA)
    AOI_name = data1 %>% select(contains('AOI'))
    aoi_n = which(colnames(data) == (colnames(AOI_name)[1]))
    for (j in (c(aoi_n:(ncol(data1)-1)))) {
      for (i in which(data1[,j] == 1)) {
        data1$AOI.Name[i] = colnames(data1)[j]
      }
    }
    data1$AOI.Name = substring(data1$AOI.Name, 10) # data$AOI.Name = substring(data$AOI.Name, 10, nchar(data$AOI.Name)-1)
    data = data1
  }
  if (any(colnames(data) == "AOI.Name") == FALSE) {
    stop("This Analysis is not appropriate for your data. Choose data with AOI.")
  }
  if(!is.element(object, unlist(data$Stimulus))) {
    stop("Incorrect object.")
  }
  if(eye_tracker == "SMI") {
      if(!is.element(participant, unlist(data$Participant))) {
    stop("Incorrect Participant.")
      }
    data1 = subset(data, Stimulus == object & Participant == participant) # subset
    data1 = na.omit(data1) # omit NA
  }
  dataX = data1$Fixation.Position.X..px.   # X
  dataYN = data1$Fixation.Position.Y..px.   # Y
  dataY = -dataYN # transform Y
  duration = data1$Event.Duration..ms. # duration of fixation
  AOI = data1$AOI.Name             # mista zajmu
  data2 = data.frame(dataX, dataYN, dataY, duration, AOI) # vyber jen dulezitych promennych
  n = length(data2$dataX)
  TD = sum(data2$duration)
  dwell = aggregate(.~AOI,data=data2,sum) # soucet doby fixaci v jednotlivych oblastech
  dwell$rel_dur = dwell$duration/TD # relativni doba delky fixaci
  basic = list(theme_bw(), scale_y_continuous(breaks= pretty_breaks()), theme(plot.title = element_text(size=title_size, hjust = 0.5),
                                                                              axis.text.x = element_text(size = x_size, angle = x_angle),
                                                                              axis.text.y = element_text(size = y_size, angle = y_angle),
                                                                              axis.title.x = element_text(size = x_labsize),
                                                                              axis.title.y = element_text(size = y_labsize),
                                                                              legend.position = "none"))
  if (count == "absolute") { # absolutni pocty
    if (same_col == TRUE) { # jedna barva
      plot_1 = ggplot(data.frame(data2), aes(x=AOI)) +
        geom_bar(stat = "Count", fill = bar_col, width = bar_width) + labs(title = "Count of Fixations in AOI", x = "Areas of Interest", y = "Count") +
        basic
      plot_2 = ggplot(data = dwell, aes(x = AOI, y = duration)) + geom_bar(stat='identity', fill = bar_col, width = bar_width) +
        labs(title = "Dwell Time", x = "Areas of Interest", y = "Duration of Fixations [ms]") +
        basic
    }
    if (same_col == FALSE) { # vice barev
      if (col_man == TRUE) { # manualne zadane barvy
        plot_1 = ggplot(data.frame(data2), aes(x=AOI, fill = AOI)) +
          geom_bar(stat = "Count", width = bar_width) + labs(title = "Count of Fixations in AOI", x = "Areas of Interest", y = "Count") +
          scale_fill_manual(values=col_set) + basic
        plot_2 = ggplot(data = dwell, aes(x = AOI, y = duration, fill = AOI)) + geom_bar(stat='identity', width = bar_width) +
          labs(title = "Dwell Time", x = "Areas of Interest", y = "Duration of Fixations [ms]") +
          scale_fill_manual(values=col_set) + basic
      }
      if (col_man == FALSE) { # automaticky zadane barvy
        plot_1 = ggplot(data.frame(data2), aes(x=AOI, fill = AOI)) +
          geom_bar(stat = "Count", width = bar_width) + labs(title = "Count of Fixations in AOI", x = "Areas of Interest", y = "Count") +
          basic
        plot_2 = ggplot(data = dwell, aes(x = AOI, y = duration, fill = AOI)) + geom_bar(stat='identity', width = bar_width) +
          labs(title = "Dwell Time", x = "Areas of Interest", y = "Duration of Fixations [ms]") +
          basic
      }
    }
  }
  if (count == "relative") { # relativni pocty
    if (same_col == TRUE) { # jedna barva
      plot_1 = ggplot(data.frame(data2), aes(x=AOI)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), fill = bar_col, width = bar_width) + labs(title = "Relative Count of Fixations in AOI", x = "Areas of Interest", y = "Relative Count") +
        basic
      plot_2 = ggplot(data = dwell, aes(x = AOI, y = rel_dur)) + geom_bar(stat='identity', fill = bar_col, width = bar_width) +
        labs(title = "Relative Dwell Time", x = "Areas of Interest", y = "Relative Duration") +
        basic
    }
    if (same_col == FALSE) { # vice barev
      if (col_man == TRUE) { # manualne zadane barvy
        plot_1 = ggplot(data.frame(data2), aes(x=AOI, fill = AOI)) +
          geom_bar(aes(y = (..count..)/sum(..count..)), width = bar_width) + labs(title = "Relative Count of Fixations in AOI", x = "Areas of Interest", y = "Relative Count") +
          scale_fill_manual(values=col_set) + basic
        plot_2 = ggplot(data = dwell, aes(x = AOI, y = rel_dur, fill = AOI)) + geom_bar(stat='identity', width = bar_width) +
          labs(title = "Relative Dwell Time", x = "Areas of Interest", y = "Relative Duration") +
          scale_fill_manual(values=col_set) + basic
      }
      if (col_man == FALSE) { # automaticky zadane barvy
        plot_1 = ggplot(data.frame(data2), aes(x=AOI, fill = AOI)) +
          geom_bar(aes(y = (..count..)/sum(..count..)), width = bar_width) + labs(title = "Relative Count of Fixations in AOI", x = "Areas of Interest", y = "Relative Count") +
          basic
        plot_2 = ggplot(data = dwell, aes(x = AOI, y = rel_dur, fill = AOI)) + geom_bar(stat='identity', width = bar_width) +
          labs(title = "Relative Dwell Time", x = "Areas of Interest", y = "Relative Duration") +
          basic
      }
    }
  }
  title = ggpubr::text_grob(paste("Participant", participant), size = top_size, face = "bold")
  grid.arrange(plot_1, plot_2, nrow = 1, top = title) # rozlozeni a pridani nadpisu
}
