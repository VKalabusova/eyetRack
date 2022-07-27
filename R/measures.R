#' Recurrence Quantification Analysis Measures
#'
#' Calculate RQA Measures.
#'
#' @details Visualization of recurrent fixations can often predispose to subjective bias when evaluating a set of results. For that reason, we used recurrence quantification analysis measures, which allow us to quantify data displayed in the recurrence plot. Using RQA, we can compare different tasks or compare multiple participants.
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
#' @param participant the name of a participant or participants (character or vector of characters)
#' @param method the name of used method ("fixed distance", "fixed grid", "AOI"). Default value "fixed distance". (character)
#' @param r radius of circle (fixed distance), size of square (fixed grid)' @param size dimension of stimulus (c(width, height)). Default value c(1920, 1200).
#' @param size size of stimulus (vector - width, height). Default c(1920, 1200).
#' @param L length of diagonal, horizontal and vertical lines. Default value set to 2.
#' @param time involve duration of fixations (TRUE/FALSE). Default value FALSE.
#'
#' @return data frame that contains of
#' \itemize{
#'   \item R - the sum of recurrences in the upper triangle of the recurrence plot/matrix.
#'   \item REC - Recurrence (\%) - recurence measure represents, for a sequence of N fixations, the percentage of recurrent fixations (i.e., how often observers refixate previously fixated image positions).
#'   \item DET - Determinism (\%) - the determinism measures the proportion of recurrent points forming diagonal lines and represents repeating gaze patterns in the recurrence plot. The minimum line length of diagonal line elements was set to L = 2. The length of the diagonal line element reflects the number of fixations making up the repeated scan path.
#'   \item LAM - Laminarity (\%) - he laminary measure is determined by vertical and horizontal lines. Vertical lines represent areas that were fixated first in a single fixation and then rescanned in detail over consecutive fixations at a later time (e.g., several fixations later), and horizontal lines represent areas that were first scanned in detail and then refixated briefly later in time. Again, we set the minimum line lengths of vertical and horizontal lines to L = 2.
#'   \item CORM - Center of Recurrent Mass - this measure indicates approximately where in time most of the recurrent points are situated. Small corm values indicate that refixations tend to occur close in time, whereas large corm values indicate that refixations tend to occur widely separated in time.
#' }
#'
#' @importFrom rapportools is.empty
#'
#' @examples measures(data_SMI, "SMI", "09-M1-CX-SI-VE.jpg", "P16", "Fixed distance")
#' @examples measures(data_Tobii, "Tobii", "10.jpg", "Participant19", "AOI", size = c(1920, 1080), time = TRUE)
#'
#' @export
measures = function(data, eye_tracker = "SMI", object, participant, method = "Fixed distance", r = 60, size = c(1920, 1200),  L = 2, time = FALSE) {
  '%!in%' = function(x,y)!('%in%'(x,y))
  measures_total = data.frame()
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
      data1 = cbind(data, AOI.Name = NA)
      data1_AOI <- which(grepl('AOI', colnames(data1)) == T)
      data1_AOI <- data1_AOI[1]
      for (j in (data1_AOI:(ncol(data1)-1))) {
        for (i in which(data1[,j] == 1)) {
          data1$AOI.Name[i] = colnames(data1)[j]
        }
      }
      data1$AOI.Name = substring(data1$AOI.Name, 10, nchar(data1$AOI.Name)-1)
      data = data1
    }
    if(object %!in% unique(levels(data$Stimulus))){
      stop("Incorrect object.")
    }

    if (eye_tracker == "SMI") {
      if(participant[i] %!in% unique(levels(data$Participant))) {
        stop("Incorrect Participant.")
      }
      data1 = subset(data, Stimulus == object & Participant == participant[i]) # subset
      data1 = na.omit(data1) # omit NA
    }
  if ((method == "AOI") & (any(colnames(data1) == "AOI.Name") == FALSE)) {
    stop("Unappropriate method for your data. Choose another method.")
  }

    x = data1$Fixation.Position.X..px.   # X
    yn = data1$Fixation.Position.Y..px.   # Y
    y = -yn # transform Y
    dur = data1$Event.Duration..ms. # duration of fixation
    n = length(x)
    TD = sum(dur)
    if (any(colnames(data1) == "AOI.Name") == TRUE) { # mame zadane oblasti zajmu
      AOI = data1$AOI.Name             # mista zajmu
      data_s = data.frame(x, yn, y, dur, AOI, AOI_numeric = AOI) # vyber jen dulezitych promennych
      data_s$AOI_numeric = as.factor(as.character(data_s$AOI_numeric))
      AOI = as.factor(as.character(AOI))
      levels(data_s$AOI_numeric) = c(c(1:length(levels(AOI))))  # zmena na cisla, ktere jsou as.character
      data_s$AOI_numeric = as.numeric (data_s$AOI_numeric)
    }
    if (any(colnames(data1) == "AOI.Name") == FALSE) {
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
    x = recurrence_matrix(data_s, method, r,  size, time)
    x = as.data.frame(x)
    f = function (v, time) {
        if (time == FALSE) {
          v2 = c(0,v,0) # prilepime z kazde strany 0 (ulehci nam to praci)
          d = diff(v2) # 1. diference (zacatek je 1, konec -1)
          beg = which(d == 1)
          end = which(d == -1)
          l = end - beg # urceni delka
          s = sum(l[l > (L-1)]) # vyber DHV delsich nez 1 (tj. 2+)
        }
        if (time == TRUE) {
          m = v
          m[m != 0] = 1
          m2 = c(0,m,0)
          d = diff(m2)
          beg = which(d == 1)
          end = which(d == -1)
          l = end - beg # urceni delka
          l1 = which(l >= L) # ktere jsou pozadovane delky
          if(is.empty(l1[1]) == TRUE) {
            s = 0
          }
          else {
            beg = beg[l1]
            end = end[l1]
            s = 0
            for (i in (1:length(l1)))
              s = s + sum(v[(beg[i]:end[i]-1)])
          }
        }
        return(s)
      }

      if (time == FALSE){
        ST_v = 0
        for (j in (1:(n-2)))
          ST_v = ST_v + f(x[as.character((j+1):n), j], FALSE)

        ST_h = 0
        for (j in (3:n))
          ST_h = ST_h + f(t(x[as.character(j), 1:(j-1)]), FALSE)

        pomocna = outer(1:n, 1:n, FUN = "+") # pomocna matice na vyber diagonal
        ST_d = 0
        for (j in (3:n))
          ST_d = ST_d + f(x[pomocna == j], FALSE)

        R = 0   # pocet 1 v hornim trojuhelniku
        for (i in (1:(n-1))) {
          for (j in ((i+1):n)) {
            R =  R + sum(x[as.character(i),j])
          }
        }
        REC = 100*(2*R/(n*(n-1))) # procento rekurentnich bodu
        DET = 100*(ST_d/R)
        LAM = 100*(ST_h + ST_v)/(2*R)
        CORM = 0
        for (i in (1:(n-1))) {
          for (j in ((i+1):n)) {
            CORM =  CORM + (((j-i)*x[as.character(i),j]))/((n-1)*R)* 100
          }
        }
      }
      if (time == TRUE)  {
        ST_v = 0
        for (j in (1:(n-2)))
          ST_v = ST_v + f(x[as.character((j+1):n), j], TRUE)

        ST_h = 0
        for (j in (3:n))
          ST_h = ST_h + f(t(x[as.character(j), 1:(j-1)]), TRUE)

        pomocna = outer(1:n, 1:n, FUN = "+") # pomocna matice na vyber diagonal
        ST_d = 0
        for (j in (3:n))
          ST_d = ST_d + f(x[pomocna == j], TRUE)

        R = 0   # pocet 1 v hornim trojuhelniku
        for (i in (1:(n-1))) {
          for (j in ((i+1):n)) {
            R =  R + sum(x[as.character(i),j])
          }
        }
        REC = 100*R/((n-1)*TD)
        DET = (100/R)*(ST_d)
        LAM = (100/(2*R))*(ST_h + ST_v)
        CORM = 0
        for (i in (1:(n-1))) {
          for (j in ((i+1):n)) {
            CORM =  CORM + 100*(((j-i)*x[as.character(i),j]))/(((n-1)^2)*TD)
          }
        }
      }
      R[is.nan(R)] = 0
      REC[is.nan(REC)] = 0
      DET[is.nan(DET)] = 0
      LAM[is.nan(LAM)] = 0
      CORM[is.nan(CORM)] = 0
      df = data.frame(R = R, REC = REC, DET = DET, LAM = LAM, CORM = CORM)
      df = as.data.frame(df)
      measures_total = rbind(measures_total,df)
    }
    measures_tot = cbind("Participant" = participant, measures_total)
    return(measures_tot)
  }
