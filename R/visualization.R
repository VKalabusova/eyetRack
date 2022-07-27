#' Visualization of fixation
#'
#' This function offers visualization of the scanpath above the stimulus. You can choose a method of visualization. You have following options:
#' \itemize{
#'     \item Simple - visualization of scanpath
#'     \item Simple with time - visualization of scanpath with duration of fixations
#'     \item Fixed distance - visualization of scanpath, including a circle with a radius of your choice
#'     \item Fixed Grid - visualization of scanpath with a grid (you can choose the size of the dimensions)
#'     \item AOI - offers visualization of scanpath. The colours of fixations vary based on its placement within different AOIs
#'     \item AOI with time - offers same visualization as the AOI method but also includes duration of fixations
#' }
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
#' @param object the name of a stimulus (character)
#' @param participant the name of a participant (character)
#' @param method visualization method - "Simple", "Simple with time", "Fixed distance", "Fixed grid", "AOI". Default "Simple"
#' @param fig if you want to put image of stimulus under plot (TRUE/FALSE). Default FALSE.
#' @param img image of stimulus (.jpg, .png). If fig = T and method is not with AOI.
#' @param img_AOI image of stimulus with AOI (.jpg, .png). If fig = T & method = "AOI" ("AOI with time").
#' @param scaling size of bubble
#' @param r radius of circle (fixed grid), size of square (fixed grid)
#' @param size size of stimulus (vector - width, height). Default c(1920, 1200)
#' @param col_man for AOI methods. If you do not want to set colours of points manually (FALSE). If you want to set colours of points manually (TRUE). Default value is FALSE.
#' @param col_set vector of N colours (N - number of unique AOIs in dataset). If col_man = T.
#' @param point_size size of points
#' @param point_col colour of points (for methods - simple, simple wt, fixed distance, fixed grid)
#' @param bubble_col colour of bubble representing relative duration of fixation (only for simple with time)
#' @param title_size title font size
#' @param x_labsize x lab font size
#' @param y_labsize y lab font size
#' @param legend_size legend font size (AOI methods)
#' @param text_size font size in plot
#' @param sac_size size of saccades
#' @param circle_size size of circles (fixed distance)
#' @param grid_size size of grid (fixed grid)
#' @param lightness lightness of bubbles representing relative duration of fixation (only for simple with time and AOI with time)
#'
#' @importFrom jpeg readJPEG
#' @import ggrepel
#' @import grid
#' @importFrom ggforce geom_circle
#'
#' @return Plot of scanpath according to visualization method of your choice.
#'
#' @examples visualization(data_SMI, "SMI","09-M1-CX-SI-VE.jpg", "P16", method = "Simple", point_size = 3, text_size = 8)
#' @examples visualization(data_Tobii, "Tobii","10.jpg", "Participant19", method = "AOI", size = c(1920, 1080))

#' @export

visualization = function (data, eye_tracker = "SMI", object, participant, method = "Simple", fig = FALSE, img = NULL, img_AOI = NULL,  scaling = 100, r = 60 , size = c(1920, 1200), col_man = F, col_set = c(), point_size = 2, point_col = "steelblue", bubble_col = "steelblue", title_size = 15, x_labsize = 10, y_labsize = 10, legend_size = 10,  text_size = 5, sac_size = 0.2, circle_size = 0.1, grid_size = 0.3, lightness = 0.4) {
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
    data1_AOI <- which(grepl('AOI', colnames(data1)) == T)
    data1_AOI <- data1_AOI[1]
    for (j in (data1_AOI:(ncol(data1)-1))) {
      for (i in which(data1[,j] == 1)) {
        data1$AOI.Name[i] = colnames(data1)[j]
      }
    }
    data = data1
    data$AOI.Name = substring(data$AOI.Name, 10, nchar(data$AOI.Name)-1)
  }
  if(!is.element(object, unlist(data$Stimulus))) {
    stop("Incorrect object.")
  }
  if(method == "AOI" && is.null(img_AOI) && fig == TRUE) {
    stop("Input image with AOI, choose other method or fig = F.")
  }
  if((method == "Fixed distance"||method == "Fixed grid") && is.null(img) && fig == TRUE) {
    stop("Input image, choose other method or fig = F.")
  }
  if ((method == "AOI") & (any(colnames(data) == "AOI.Name") == FALSE)) {
    stop("Unappropriate method for your data. Choose another method.")
  }
  if(eye_tracker == "SMI") {
      if(!is.element(participant, unlist(data$Participant))) {
    stop("Incorrect Participant.")
  }
    data = subset(data, Stimulus == object & Participant == participant) # subset
    data = na.omit(data) # NA
  }
  x = data$Fixation.Position.X..px.   # X
  yn = data$Fixation.Position.Y..px.   # Y
  y = -yn # transform Y
  duration = data$Event.Duration..ms. # duration of fixation
  n = length(x)
  TD = sum(duration)
  AOI = data$AOI.Name
  options(ggrepel.max.overlaps = Inf) # nastaveni kvuli prekryvani popisku
  basic = list(geom_path (col = "black", size = sac_size), geom_text_repel(aes(label = c(1:n)), col = "black", position=position_jitter(width=1,height=1), size = text_size),
               labs(title = paste("Scanpath ", participant, sep = ""), x = "X Position [px]", y = "Y Position [px]"),
               scale_x_continuous (expand = c(0,0), limits = c(0, size[1]), labels = NULL),
               scale_y_continuous (expand = c(0,0), limits = c(-size[2], 0), labels = NULL),
               theme(
                 panel.background=element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 axis.title.x = element_text(size = x_labsize),
                 axis.title.y = element_text(size = y_labsize),
                 legend.text = element_text(size = legend_size),
                 legend.title=element_text(size=(legend_size + 2), face = "bold"),
                 plot.title = element_text(hjust = 0.5, size = title_size)
               )
  )
  if (method == "Simple") {
    if (fig == TRUE){
      plot1 = ggplot (data, aes (x, y)) + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))) +
        geom_point(col = point_col, size = point_size) + basic
    }
    else if (fig == FALSE) {
      plot1 = ggplot (data, aes (x, y)) + geom_point(col = point_col, size = point_size) + theme_bw() + basic
    }
  }
  if (method == "Simple with time"){
    if (fig == TRUE) {
      plot1 = ggplot(data,aes(x,y)) + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))) +
        geom_point(size = (duration/TD)*scaling, alpha = lightness, shape = 20, colour = bubble_col) +
        geom_point(size = point_size, col = point_col) + basic
    }
    if (fig == FALSE) {
      plot1 = ggplot(data,aes(x,y)) + geom_point(size = (duration/TD)*scaling, alpha = lightness, shape = 20, colour = bubble_col) +
        geom_point(size = point_size, col = point_col) + theme_bw() + basic
    }
  }

  if (method == "Fixed distance") {
      if (fig == TRUE){
        plot1 = ggplot (data, aes (x, y)) + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))) +
          geom_point(col = point_col, size = point_size) + geom_circle(data = NULL, aes(x0 = x, y0 = y, r = r), size = circle_size, inherit.aes=FALSE) + basic
      }
      else if (fig == FALSE) {
        plot1 = ggplot (data, aes (x, y)) + geom_point(col = point_col, size = point_size) +
          geom_circle(data = NULL, aes(x0 = x, y0 = y, r = r), size = circle_size, inherit.aes=FALSE) + theme_bw() + basic
      }
    }
  if (method == "Fixed grid") {
      if (fig == TRUE) {
        plot1 = ggplot(data, aes (x,y)) + annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")) , xmin = 0, xmax = size[1], ymin = -size[2], ymax = 0) +
          geom_point(col = point_col, size = point_size) +
          geom_hline (yintercept=seq(-size[2], 0, by = r), col = "black", size = grid_size) +
          geom_vline (xintercept=seq(0, size[1], by = r), col = "black", size = grid_size) + basic
      }
      if (fig == FALSE){
        plot1 = ggplot(data, aes (x,y)) + geom_point(col = point_col, size = point_size) +
          geom_hline (yintercept=seq(-size[2], 0, by = r), col = "black", size = grid_size) +
          geom_vline (xintercept=seq(0, size[1], by = r), col = "black", size = grid_size) + theme_bw() + basic
      }
    }
  if (method == "AOI") {
      if (fig == TRUE) {
        if (col_man == TRUE) {
          plot1 = ggplot(data, aes(x,y)) + annotation_custom(rasterGrob(img_AOI, width=unit(1,"npc"), height=unit(1,"npc"))) +
            scale_color_manual(values = col_set) + geom_point(aes(color = AOI), size = point_size) + basic
        }
        if (col_man == FALSE) {
          plot1 = ggplot(data, aes(x,y)) + annotation_custom(rasterGrob(img_AOI, width=unit(1,"npc"), height=unit(1,"npc"))) +
            geom_point(aes(color = AOI), size = point_size) + basic
        }
      }
      if (fig == FALSE) {
        if (col_man == TRUE) {
          plot1 = ggplot(data, aes(x,y)) + scale_color_manual(values = col_set) +
            geom_point(aes(color = AOI), size = point_size) + theme_bw() + basic
        }
        if (col_man == FALSE) {
          plot1 = ggplot(data, aes(x,y)) + geom_point(aes(color = AOI), size = point_size) + theme_bw() + basic
        }
      }
  }
  if (method == "AOI with time") {
      if (fig == TRUE) {
        if (col_man == TRUE) {
          plot1 = ggplot(data, aes(x,y)) + annotation_custom(rasterGrob(img_AOI, width=unit(1,"npc"), height=unit(1,"npc"))) +
            geom_point(aes(color = AOI), size = (duration/TD)*scaling, alpha = lightness) + scale_color_manual(values = col_set) +
            geom_point(aes(color = AOI), size = point_size) + basic
        }
        if (col_man == FALSE) {
          plot1 = ggplot(data, aes(x,y)) + annotation_custom(rasterGrob(img_AOI, width=unit(1,"npc"), height=unit(1,"npc"))) +
            geom_point(aes(color = AOI), size = (duration/TD)*scaling, alpha = lightness) +
            geom_point(aes(color = AOI), size = point_size) + basic
        }
      }
      if (fig == FALSE) {
        if (col_man == TRUE) {
          plot1 = ggplot(data, aes(x,y)) + geom_point(aes(color = AOI), size = (duration/TD)*scaling, alpha = lightness) + scale_color_manual(values = col_set) +
            geom_point(aes(color = AOI), size = point_size) + theme_bw() + basic
        }
        if (col_man == FALSE) {
          plot1 = ggplot(data, aes(x,y)) + geom_point(aes(color = AOI), size = (duration/TD)*scaling, alpha = lightness) +
            geom_point(aes(color = AOI), size = point_size) + theme_bw() + basic
        }
      }
    }
  return(plot1)
}

