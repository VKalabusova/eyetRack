#' Recurrence
#'
#' This function returns Recurrence matrix and Recurrence plot. Two fixations are considered to be recurrent if they are close together.
#' We use 3 different methods to identify recurrent fixations:
#' \itemize{
#'    \item fixed distance - two fixations \emph{A} and \emph{B} are recurrent if they are close to each other (i.e., if the Euclidean distance \emph{d(A, B) <= r} for a fixed radius \emph{r}).
#'    \item fixed grid - fixation map overlaid by a grid with element size of \emph{r} pixels . Two fixations \emph{A} and \emph{B} are considered recurrent if they land in the same grid element.
#'    \item Areas of Interest (AOI) - two fixations \emph{A} and \emph{B} are considered recurrent if they land in the same area of interest
#' }
#'
#' Recurrence matrix is a square matrix, which includes 1 if the fixation is recurrent and 0 if it is not recurrent. Should a duration be included, the matrix will include the sum of durations instead of 1. Recurrence can be represented in a recurrence plot, which plots recurrences of a fixation sequence with itself over all possible time lags. If fixations i and j are recurrent (i.e., if r_ij = 1), then a dot is plotted at position i, j. If we are including durations as a variable, the size of a point on a plot is given by the sum of recurrence of both fixations t_i a t_j.
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
#' @param method the name of used method ("fixed distance", "fixed grid", "AOI"). Default value "fixed distance".
#' @param r radius of circle (fixed distance), size of square (fixed grid)
#' @param scaling size of bubble (representing relative time)
#' @param size size of stimulus (vector - width, height). Default c(1920, 1200)
#' @param time involve time TRUE/FALSE. Default value FALSE.
#' @param col_man for AOI methods. If you do not want to set colours of points manually (FALSE). If you want to set colours of points manually (TRUE). Default value is FALSE.
#' @param col_set vector of N colours (N - number of unique AOIs in dataset). If col_man = T.
#' @param point_size size of points
#' @param point_col colour of points (fixed distance and fixed grid)
#' @param bubble_col colour of bubble representing relative duration of fixation (fixed distance, fixed grid with time involved)
#' @param title_size title font size
#' @param legend_size legend font size
#' @param x_labsize x lab font size
#' @param y_labsize y lab font size
#' @param x_size x lab ticks font size
#' @param y_size y lab ticks font size
#' @param breaks breaks on x and y axis
#' @param lightness lightness of bubble in RP
#'
#'
#' @return list Recurrence matrix, Recurrence plot
#'
#' @importFrom Thermimage rotate90.matrix
#' @importFrom Thermimage rotate270.matrix
#' @importFrom tidyr gather
#'
#' @examples recurrence(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16", "Fixed distance", point_col = "blue", bubble_col = "steelblue", legend_size = 10, lightness = 0.2)$Rec_plot
#' @examples recurrence(data_Tobii, "Tobii", "10.jpg", "Participant19", "AOI", size = c(1920, 1080),  time = FALSE)
#'
#' @export
recurrence = function(data, eye_tracker = "SMI", object, participant, method = "Fixed distance", r = 60, scaling = 100,  size = c(1920, 1200), time = FALSE, col_man = FALSE, col_set = c(), point_size = 1.5, point_col = "darkblue", bubble_col = "steelblue", title_size = 15, legend_size = 5, x_labsize = 10, y_labsize = 10, x_size = 10, y_size = 10, breaks = 2, lightness = 0.2) {
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
    data1 = data
  }
  if(!is.element(object, unlist(data$Stimulus))) {
    stop("Incorrect object.")
  }
  if ((method == "AOI") & (any(colnames(data) == "AOI.Name") == FALSE)) {
    stop("Unappropriate method for your data. Choose another method.")
  }
  if(eye_tracker == "SMI") {
    if(!is.element(participant, unlist(data$Participant))) {
      stop("Incorrect Participant.")
    }
    data1 = subset(data, Stimulus == object & Participant == participant) # subset
    data1 = na.omit(data1) # NA
  }
  x = data1$Fixation.Position.X..px.   # X
  yn = data1$Fixation.Position.Y..px.   # Y
  y = -yn # transform Y
  dur = data1$Event.Duration..ms. # duration of fixation
  n = length(x)
  TD = sum(dur)
  if (any(colnames(data) == "AOI.Name") == TRUE) { # mame zadane oblasti zajmu
    AOI = data1$AOI.Name             # mista zajmu
    data_s = data.frame(x, yn, y, dur, AOI, AOI_numeric = AOI) # vyber jen dulezitych promennych
    data_s$AOI_numeric = as.factor(as.character(data_s$AOI_numeric))
    AOI = as.factor(as.character(AOI))
    levels(data_s$AOI_numeric) = c(c(1:length(levels(AOI))))  # zmena na cisla, ktere jsou as.character
    data_s$AOI_numeric = as.numeric (data_s$AOI_numeric)
  }
  if (any(colnames(data) == "AOI.Name") == FALSE) {
    data_s = data.frame(x,y,yn,dur)
  }
  aoi = data_s$AOI_numeric
  recurrence_matrix = function (data, method, r, size, time) {
    distance = function (x1, x2, y1, y2)    # funkce na vzdalenost mezi 2 fixacemi (eukleidovska metrika)
    {
      v = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
      return (v)
    }
    distance_area = function (o1, o2)  # funkce na vzdalenost 2 oblasti
    {
      vo = (o1 - o2)
      return (vo)
    }
    division = function (data, r, size) {
      data$row = seq(0, length = n) # pridame sloupec, ktery bude predstavovat radek
      data$col = seq(0, length = n)
      int_x = ceiling (size[1]/r) # pocet rozdeleni intervalu osy x
      int_y = ceiling (size[2]/r) # pocet rozdeleni intervalu osy y
      for (i in (1 : int_x))
        for (j in (1 : int_y))
          for (k in (1 : n)) {
            if  ((x[k] > (r*(i-1)) & x[k] <= (r*i)) & (yn[k] > (r*(j-1)) & yn[k] <= (r*j))) {(data$row[k] = i) &
                (data$col [k] = j)
            }
          }
      return(data)
    }
    df = division(data_s, r, size) # data frame s rozdelenim fixaci do ctvercu
    RM = as.data.frame(matrix(data = NA, nrow = n, ncol = n))  # do matice ukladame nase hodnoty
    for (i in (1:n))
      for (j in (1:n)) {
        if (method == "Fixed distance") {
          if (time == FALSE) {
            if (distance(x[i], x[j], y[i], y[j]) < r ) {RM [i,j] = 1
            } else {RM[i,j] = 0}  # 1 pokud je mensi nez polomer, jinak 0
          }
          else if (time == TRUE){
            if (distance (x[i], x[j], y[i], y[j]) < r ) {RM[i,j] = (dur [i] + dur [j])
            } else {RM[i,j] = 0}  # soucet casu pokud je mensi nez polomer, jinak 0
          }
        }
        else if (method == "Fixed grid"){
          if (time == FALSE){
            if ((distance_area(df$row [i], df$row [j]) == 0) & (distance_area(df$col[i], df$col[j]) == 0 )) {RM[i,j] = 1
            } else {RM[i,j] = 0}
          }
          else if (time == TRUE) {
            if ((distance_area(df$row[i], df$row[j]) == 0) & (distance_area(df$col[i], df$col [j]) == 0 )) {RM[i,j] = (dur[i] + dur[j])
            } else {RM[i,j] = 0}
          }
        }
        else if (method == "AOI") {
          if (time == FALSE){
            if (distance_area(aoi[i], aoi[j]) == 0) {RM[i,j] = 1
            } else {RM[i,j] = 0}
          }
          else if (time == TRUE){
            if (distance_area (aoi[i], aoi[j]) == 0) {RM[i,j] = (dur[i] + dur[j])
            } else {RM[i,j] = 0}
          }
        }
      }
    RM = as.data.frame(ceiling(rotate90.matrix(RM)))
    rownames(RM) = c(n:1)
    colnames(RM) = c(1:n)
    return(as.data.frame(RM))
  }

  recurrence_plot = function (data, method, time, col_man) {
    x = recurrence_matrix(data, method, r, size, time)
    x = as.data.frame(rotate270.matrix(x))
    order1 = function (x = recurrence_matrix) {
      x = cbind (c(1:n), x) # pridani sloupce s indexy radku
      colnames (x) = c("row", c(1:n)) # prejmenovani sloupcu
      S2 = gather (x, key = "column", value = "values", - "row")   # spojime do matice...radek, sloupec, hodoty
      S2 [S2 == 0] = NA   # misto 0 piseme NA
      S1 = na.omit (S2) # ze serazene matice, vybereme jenom 1 (bez NA)
      S = as.data.frame (sapply(S1, as.numeric))
    }
    x = order1(x)
    col_AOI = function(x = ordered_RM) {
      x = data.frame(x, col_AOI = 0) # pridame sloupec, ktery bude oznacovat, ve ktere oblasti zajmu, se rekurentni body nachazi
      data$AOI = as.factor(as.character(data$AOI))
      for (j in c(1:length(levels(data$AOI))))
        for (i in c(1:nrow(x)))  {
          if (is.element(x$row [i], which(data$AOI_numeric == j))) {
            x$col_AOI [i] = levels(data$AOI)[j]}
        }
      return(x)
    }
    x = col_AOI(x)
    basic = list(scale_x_continuous (limits = c(1, n), breaks = seq(1, n, breaks)),
                 scale_y_continuous (limits = c(1, n), breaks = seq(1, n, breaks)),
                 theme(plot.title = element_text(hjust = 0.5, size = title_size),
                       axis.text.x = element_text(size = x_size),
                       axis.text.y = element_text(size = y_size),
                       axis.title.x = element_text(size = x_labsize),
                       axis.title.y = element_text(size = y_labsize),
                       legend.position = "right",
                       legend.text = element_text(size = legend_size),
                       legend.title = element_text(size= (legend_size + 2), face = "bold")
                 ))
    if (time == FALSE) {
      if ((method == "Fixed distance")|(method == "Fixed grid")) {
        RP = ggplot(x, aes (row, column)) +
          geom_point(col = point_col, size = point_size) + labs (title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j") +
          theme_bw() + basic
      }
      else if (method == "AOI") {
        if (col_man == TRUE) {
          RP = ggplot(x, aes (row, column, color=as.factor(col_AOI))) +
            geom_point(aes(color = col_AOI), size = point_size) + scale_color_manual(name = "AOI", values = col_set, limits = force) +
            labs (title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j", color = "AOI") +
            theme_bw() + basic
        }
        if (col_man == FALSE){
          RP = ggplot(x, aes (row, column, color=as.factor(col_AOI))) +
            geom_point(aes(color = col_AOI), size = point_size) +
            labs (title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j", color = "AOI") +
            theme_bw() + basic

        }
      }
    }
    else if (time == TRUE){
      if ((method == "Fixed distance")| (method == "Fixed grid")) {
        RP = ggplot(x, aes (row, column)) + geom_point(color = bubble_col, alpha = lightness, size = x$values/TD*scaling) + geom_point(color = point_col, size = point_size, shape = 20) +
          labs(title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j") +
          theme_bw() + basic
      }
      else if (method == "AOI") {
        if (col_man == TRUE) {
          RP = ggplot(x, aes (row, column, color=as.factor(col_AOI))) +
            geom_point(aes(color = col_AOI), alpha = lightness, size = x$values/TD*scaling) + scale_color_manual(name = "AOI", values = col_set, limits = force) +
            labs (title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j", color = "AOI")  +
            theme_bw() + basic
        }
        if (col_man == FALSE) {
          RP = ggplot(x, aes (row, column, color=as.factor(col_AOI))) + geom_point(aes(color = col_AOI), size = point_size) +
            geom_point(aes(color = col_AOI), alpha = lightness, size = x$values/TD*scaling)  +
            labs (title = paste("Recurrence plot ", participant, sep = ""), x = "Fixation i", y = "Fixation j", color="AOI")  +
            theme_bw() + basic
        }
      }
    }
    return(RP)
  }

  RM = recurrence_matrix(data_s, method, r, size, time)
  RP = recurrence_plot(data_s, method, time, col_man)
  return(list(Rec_matrix = RM, Rec_plot = RP))
}
