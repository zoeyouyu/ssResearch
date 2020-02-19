##### Preparing packages

# install.packages("tidyverse")
# install.packages("rjson")
# install.packages("quantmod")
# install.packages("pracma")
# install.packages("changepoint")
# install.packages("purrr")

library(tidyverse)
library(rjson)
library(quantmod)
library(pracma)
library(changepoint)
library(purrr)
library(ggpubr)

###### A function that tidys up pressure values a given dataset
tidy.pressure = function(data){
  data %>%
    select(-t1:-t8) %>%
    gather(sensor, pressure, p1:p8) %>%
    mutate(sensor = as.factor(sensor))
}

###### A function that tidys up temperature values a given dataset

tidy.temp = function(data){
  data %>%
    select(-p1:-p8) %>%
    gather(sensor, temp, t1:t8) %>%
    mutate(sensor = as.factor(sensor))
}

####### A function that return JSON information in Kari Bo protocol
get.json.info = function(folder) {
  # Get the session json of this subject
  session = fromJSON(file = list.files(path = folder, pattern = "session.json", recursive = TRUE, full.names = TRUE))
  
  # Get the profile json file, so we have information about the exercises
  profile = fromJSON(file = list.files(path = folder, pattern = "profile", recursive = TRUE, full.names = TRUE))
  # or pattern = ".+(_)+.+.json"
  
  exercise_list = profile$exercise_groups[[1]]$exercises
  
  # return all
  json.info.list = list(session = session, 
                        profile = profile, 
                        exercise_list = exercise_list)
  
  json.info.list
}


###### A function to obtain all available exercise ids (as a list) in Kari Bo protocol
get_id_list = function(folder) {
  # Get the exercise list
  exercise_list = get.json.info(folder)$exercise_list
  
  # Get the exercise list length
  N = length(exercise_list)
  
  # Initialise an empty id list
  id_list = vector("list", N)
  
  # Loop through the exercise list to get every exercise id
  for (i in (1:N)) {
    id_list[[i]] = exercise_list[[i]]$id
    i = i + 1
  }
  
  id_list 
  
}

###### A function that obtain the description for a rough input exercise name (id) in Kari Bo protocol
get_description = function(id){
  
  # Choose a random(the first one) folder in the current wd (not a good idea)
  folder = list.files(pattern = "_csv")[1]
  
  id_list = get_id_list(folder)
  if (is.null(id) == TRUE) {
     return(NULL)
  }
  # Get rid of info (.participantid) after "."
  if (length(strsplit(id, "\\.")[[1]]) > 1) {
    description = "Maximum PFM Contration for 5-8 seconds."
  }
  
  else {
    id = strsplit(id, "\\.")[[1]][1]
    id = gsub("data|change", "", id)
    
    
    # Get the exercise order in the list
    id_order = grep(id, id_list)
    
    if (length(id_order) == 0) {
      description = NULL
    } else {
      
      # Get the exercise list
      exercise_list = get.json.info(folder)$exercise_list
      
      # Get the exercise
      exercise_list[[id_order]]
      
      # Get the description
      description = exercise_list[[id_order]]$activity
    }
  }
  
  description
}




###### A function of extracting start/stop time for a given exercise
# (modified from the individual subject s0007 version)
getrectime = function(folder, exercise){
  
  # Get the json info of this subject
  json.info.list = get.json.info(folder)
  session = json.info.list$session
  profile = json.info.list$profile
  
  # Get the starting time from recordings
  rec = session$recordings
  start = rec[[1]]$start
  
  # Get the order of the exercise in exercise_order
  order = grep(exercise, session$exercise_order)
  
  # Get the pfmc exercise start and stop timestamps
  exercise_start = session$exercises[[order]]$start
  exercise_stop = session$exercises[[order]]$stop
  next_one_start = session$exercises[[order+1]]$start
  
  # Subtract the initial recording start timestamp to get the approximate time
  exercise_start_time = round((exercise_start - start) / 10) * 10
  exercise_stop_time = round((exercise_stop - start) / 10) * 10
  next_one_start_time = round((next_one_start - start) / 10) * 10
  
  list(exercise_start_time, exercise_stop_time, next_one_start_time)
}


###### A function of subsetting data for a given time
getsubdata = function(data, start = min(data$rectime), stop = max(data$rectime)){
  
  if (start < min(data$rectime)){
    stop(paste0("Sorry, data missing for recording time before ", min(data$rectime), " s."))
  }
  
  if (stop > max(data$rectime)){
    stop(paste0("Sorry, data missing for recording time after ", max(data$rectime), " s."))
  }
  
  data %>% filter(rectime >= start, rectime <= stop)
}




###### A function plot pressure values vs time (seconds), 
###### where the time variable is indicating how long after the this exercise has begun
myplot = function(data, name = NULL) {
  # mutate a new time variable 
  # by subtract the recoring time varibale by its starting time (minimal value)
  # Note, there are some missingness
  data$time = data$rectime - min(data$rectime)
  
  timelength = max(data$time)
  description = NULL
  # Default time gap is 1s
  gap = 1000
  
  # Take 5s as the gap if total time is greater than 30 seconds
  if (timelength > 30000) { gap = 5000 }
  
  # Take 10s as the gap if total time is greater than 100 seconds
  if (timelength > 100000) { gap = 10000 }
  
  # Take 50s as the gap if total time is greater than 30 seconds
  if (timelength > 500000) { gap = 50000 }
  

  if (is.null(get_description(name)) == FALSE) {
    description = paste("Instruction:", get_description(name))
  }
  

  data %>% 
    tidy.pressure() %>%
    ggplot(aes(x = time, y = pressure, color = sensor)) +
    geom_line(position = position_dodge(width = 0.2)) +
    scale_x_discrete(limits = seq(0, round(max(data$time)/1000) * 1000, gap), 
                     labels = seq(0, round(max(data$time)/1000), gap/1000)) +
    labs(x = "Time (s)", y = "Pressure (mmHg)",
         title = paste("Pressure trace from", name)) +
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}


###### A function plot the pressure change vs rectime (ms) (rectime is the original recording time variable in data)
plot2 = function(data, name = NULL){
  if (is.null(name) == TRUE) 
  {description = NULL} 
  else 
  {description = paste("Instruction:", get_description(name))}
  
  data %>% 
    tidy.pressure() %>%
    ggplot(aes(x = rectime, y = pressure, 
               group = sensor, color = sensor)) +
    geom_line(position = position_dodge(width = 0.2)) +
    labs(x = "Recording Time (ms)", y = "Pressure (mmHg)",
         title = paste("Pressure trace from", name)) +   
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}


###### A function plot the temperature change vs rectime (ms) (rectime is the original recording time variable in data)

plot.temp = function(data, name = NULL){
  data$time = data$rectime - min(data$rectime)
  
  timelength = max(data$time)

  # Default time gap is 1s
  gap = 1000
  
  # Take 5s as the gap if total time is greater than 30 seconds
  if (timelength > 30000) { gap = 5000 }
  
  # Take 10s as the gap if total time is greater than 100 seconds
  if (timelength > 100000) { gap = 10000 }
  
  # Take 50s as the gap if total time is greater than 30 seconds
  if (timelength > 500000) { gap = 50000 }
  
  data %>% 
    tidy.temp() %>%
    ggplot(aes(x = time, y = temp, 
               group = sensor, color = sensor)) +
    geom_line() + 
    scale_x_discrete(limits = seq(0, round(max(data$time)/1000) * 1000, gap), 
                     labels = seq(0, round(max(data$time)/1000), gap/1000)) +
    labs(x = "Time (s)", y = "Tempetarue (Celsius)",
         title = paste("Temperature from", name)) +   
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}
###### A function to remove baseline values

baseline = function(data, relaxdata){
  newdata = sweep(data[-1], 2, colMeans(relaxdata[-1]))
  newdata$rectime = data$rectime
  newdata
}


###### A run-everything process function - return the a list of 4 pfmc datasets 
######  (1 whole original, 1 whole change, 1 pfmc original, 1 pfmc change)
process = function(folder, whole = NULL){
  # Get the data
  whole = read.csv(list.files(path = folder, pattern = ".csv", recursive = TRUE, full.names = TRUE))
  
  # Get the json info of this participant
  json.info.list = get.json.info(folder)
  session = json.info.list$session
  profile = json.info.list$profile
  
  # Get participant id
  participant_id = json.info.list$session$patient_id
  
  # Rename columns
  colnames(whole) = c("rectime", paste0("p", 1:8), paste0("t", 1:8))
  # Drop temperature data
  # data = whole %>% select(-(t1:t8))
  data = whole
  # Get the start/end time of the pfmc exercise, and the start time for next exercise
  name = "pfmc"
  rectime = getrectime(folder, name)
  
  # Get pfmc data
  pfmcdata = getsubdata(data, rectime[[1]], rectime[[3]])
  
  # Get the resting period - stable data
  attempt3 = getsubdata(data, rectime[[1]], rectime[[1]] + 15000)
  
  # Take out the baseline from data
  newdata = baseline(data, attempt3)
  
  # Subset the pfmc data
  pfmcchange = getsubdata(newdata, rectime[[1]], rectime[[3]])
  
  # Retutn the whole dataset, pfmc dataset, one original, one change
  datalist = list(data, newdata, pfmcdata, pfmcchange)
  
  names(datalist) = c(paste0("whole.participant", participant_id),
                      paste0("wholechange.participant", participant_id),
                      paste0("pfmcdata.participant", participant_id), 
                      paste0("pfmcchange.participant", participant_id))
  
  return(datalist)
  
}


##### A function that filter out folders contain data (csv files)
find.data.folders = function(all.folders) {
  data.folders = c()
  for (folder in all.folders) {
    
    # ignore the Rproj folder
    if (!grepl("Rproj", folder)) {
      
      # list all files in this folder
      filenames = list.files(path = folder, recursive = TRUE)
      
      # if this folder has csv files, save it
      if (any(grepl("csv", filenames))) {
        
        data.folders = c(data.folders, folder)
      }
    }
  }
  
  return(data.folders)
}


##### A function that process all exercise info from JSON file
exercise.info = function(folder) {
  # Get session information in JSON file
  session = unlist(fromJSON(file = list.files(path = folder, pattern = "session.json", 
                                              recursive = TRUE, full.names = TRUE)))
  
  # Find lines where exercise info locate at
  lines = grep("exercise selected", session)
  
  jsontime = unname(session[c(lines, lines + 1)])

  n = length(jsontime)
  
  return(list(jsontime[1:(n/2)], jsontime[(n/2 + 1):n]))
}

##### A function that finds when does a certain exercise begins and the next exercise begins
# Defult exercise is `Squeeze and Hold`
getjsontime = function(folder, interested_exercise = "Squeeze and Hold") {
  
  # Get session information in JSON file
  session = unlist(fromJSON(file = list.files(path = folder, pattern = "session.json", 
                                              recursive = TRUE, full.names = TRUE)))
  
  # Get exercise string
  exercise.string = exercise.info(folder)[[1]]
  lines = grep("exercise selected", session)
  
  # Find where interested_exercise info locate in exercise.string
  interested_exercise_order = grep(interested_exercise, exercise.string)
  interested_exercise_line = lines[interested_exercise_order]
  
  n = length(interested_exercise_line)
  # Special case
  # If strangely, We have more than 1 interested_exercise_line 
  # It means this exercise had been selected more than once, the user might pause the exercise in between
  
  # Check if "Pause" had been pressed between 
  # If Paused, take the last line
  if (n > 1) {
    
    if (any(grepl("Pause", session[interested_exercise_line[1]:interested_exercise_line[n]]))) {
      
      # Choose the last line - interested_exercise_line[n]
      interested_exercise_line = interested_exercise_line[n]
      
      # Get the next exercise order
      interested_exercise_order = interested_exercise_order + n - 1
    }
  }
  
  # The next line after interested_exercise_line is the time information
  exercise_start_time = as.numeric(session[interested_exercise_line + 1])
  
  # Find where next exercise info locate in exercise.string
  next_exercise_name = exercise.string[interested_exercise_order + 1]
  next_exercise_line = lines[interested_exercise_order + 1]
  
  # The next line is the time information
  next_start_time = as.numeric(session[next_exercise_line + 1])
  
  jsontime = c(exercise_start_time, next_start_time)
  names(jsontime) = c(interested_exercise, next_exercise_name)
  
  return(jsontime)
}
  
### A helper function that define the column names for each df
define.col = function(df) {
  colnames(df) = c("rectime", paste0("p", 1:8), paste0("t", 1:8))
  df
}



### A helper function that get all dfs from the files
get.df.list = function(folder){
  
  # Get the all.csv files in this folder
  all.csv = list.files(folder, pattern = ".csv$", full.names = TRUE)
  
  if (any(grepl("all", all.csv))) {
    # Ignore _all.csv files
    all.csv = all.csv[-grep("all", all.csv)]
  }
  
  # Read in all data 
  df.list = lapply(all.csv, read.csv)
  names(df.list) = paste0("df", 1:length(df.list))
 
  # Define the column names for all dfs
  df.list = lapply(df.list, define.col)
  
  return(df.list)
} 


### A helper function that combines all df
combinedf = function(df.list, timegap = 20) {
  whole = do.call("rbind", df.list[-1])
  rownames(whole) = 1:nrow(whole)
  whole$rectime = seq(0, (nrow(whole) - 1)*timegap, timegap)
  
  return(whole)
}

  
### A helper function that plots pressure and temp side by side
side.by.side = function(pressuredata, tempdata, name = "the whole recording") {
  # pressure plot (p)
  p = myplot(pressuredata, name = name)
  
  # temperature plot (t)
  t = plot.temp(tempdata, name = name)
  
  print(ggarrange(p + rremove("xlab"), t,
            ncol = 1, nrow = 2, align = "v", 
            common.legend = TRUE, legend = "right"))
}





### A helper function that smooths the peaks and return 2 pretty graphs
get.smooth.line = function(tidydata, span = 0.01) {
  lo.whole = loess(peak ~ rectime, data = tidydata, span = span)
  plot(tidydata$rectime, tidydata$peak, pch = 21, cex = 0.5)
  
  lines(tidydata$rectime[order(tidydata$rectime)],
        predict(lo.whole)[order(tidydata$rectime)],
        col = "red", lwd = 2)
  
  # Get smoothed.peak.df
  smoothed.peak.df = unique(data.frame(rectime = tidydata$rectime[order(tidydata$rectime)], 
                                       peak = predict(lo.whole)[order(tidydata$rectime)]))
  smoothed.peak.df$id = 1:nrow(smoothed.peak.df)
  
  # Plot the smooth line
  g = ggplot(data = tidydata) +
    geom_line(aes(x = rectime, y = pressure, color = sensor)) +
    geom_line(data = smoothed.peak.df, aes(x = rectime, y = peak)) +
    labs(x = "Recording Time (in s)", y = "Pressure Change (in mm Hg)",
         title = "Pressure trace from all sensors with smoothed line of peak") +   
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
  print(g)
  #ggsave("Colored Pressure traces with Smoothed Line.png", width = 10, height = 5)
  
  return(smoothed.peak.df)
}



#### A function that picks up the peaks and print plots
get.peaks = function(change, smoothed.peak.df, timelength = 5000, minpeakdistance = 500){
  # Before calling `findpeaks` function, set a threshold called noise to be ignored (not picked up as a peak - a potential event)
  # Get the baseline noise from the first 5 seconds of data
  
  noise = change %>% 
    tidy.pressure() %>%
    filter(rectime < timelength) %>%
    summarise(maxdiff = max(pressure) - min(pressure)) %>%
    as.numeric()
  
  change %>%
    filter(rectime < timelength) %>% 
    plot2(name = paste("the first", timelength/1000, "seconds  - as our baseline noise"))  %>%
    print()
  
  top_n_peaks = findpeaks(smoothed.peak.df$peak, minpeakdistance = minpeakdistance, minpeakheight = noise)
  top_n_peaks_indices = sort(top_n_peaks[, 2])
  
  # Check how many peaks we got
  n_peaks = length(top_n_peaks_indices)
  
  plot(smoothed.peak.df$peak, type = "l")
  points(top_n_peaks[, 2], top_n_peaks[, 1], pch = 15, col = "red")
  text(top_n_peaks[, 2] + 300, top_n_peaks[, 1] + 2, as.character(top_n_peaks[, 2]))
  
  return(n_peaks)
}




########## A function that gets the greedy fit
get.greedy.fit = function(smoothed.peak.df, n_peaks, Q = n_peaks*2) {
  all.binseg = cpt.mean(smoothed.peak.df$peak, method = "BinSeg", Q = Q)
  greedy.fit = cpts(all.binseg)
  # These are all the mean values for each segment
  #param.est(all.binseg)
  
  # Plot all change points
  plot(all.binseg)
  return(greedy.fit)
}




########## A function that drops a few points
drop.points = function(greedy.fit, dropfirst = 0, droplast = 0){
  mypoints = greedy.fit[(1+dropfirst):(length(greedy.fit)-droplast)]
}





########### A function that ask the user how the contraction needs to shifted, then trims the contractions and plot
# `start.go.left` contains the number of contractions whose starting point needs to be shifted to the left, 
# `start.go.left.size`  indicates how many points it needs to be shifted, usaully 10, or 30, or 50

how.to.shift = function(change, mypoints, n_event = 20, save = FALSE,
                        start.go.left = NULL, start.go.left.size = 0,
                        start.go.right = NULL,  start.go.right.size = 0,
                        end.go.left = NULL, end.go.left.size = 0,
                        end.go.right = NULL, end.go.right.size = 0) {
  
  raw.start.end.rectime = change$rectime[mypoints]
  
  start.end.rectime = raw.start.end.rectime
  
  event_start = seq(1, n_event, 1)*2 - 1
  event_end = seq(1, n_event, 1)*2
  
  
  # Shift the starting time of certain contractions (stored in start.go.left) `start.go.left.size` points to the right
  mypoints[event_start[start.go.left]] = mypoints[event_start[start.go.left]] - start.go.left.size
  
  
  # Shift the starting time of certain contractions (stored in start.go.right) `start.go.right.size` points to the right
  mypoints[event_start[start.go.right]] = mypoints[event_start[start.go.right]] + start.go.right.size
  
  
  # Shift the ending time of certain contractions (stored in end.go.left) `end.go.left.size` points to the left
  mypoints[event_end[end.go.left]] = mypoints[event_end[end.go.left]] - end.go.left.size
  
  
  # Shift the ending time of certain contractions (stored in end.go.right) `end.go.right.size` points to the right
  mypoints[event_end[end.go.right]] = mypoints[event_end[end.go.right]] + end.go.right.size
  
  
  start.end.rectime = change$rectime[mypoints] 
  
  ##### Let's have a look at each individual event (just in squeeze and hold )
  for (i in 1:n_event) {
    start = start.end.rectime[i*2 - 1]
    end = start.end.rectime[i*2]
    g = myplot(getsubdata(change, start, end), name = paste0("Event No.", i)) + theme_classic()
    print(g)
    
    if (save == TRUE) {
      print(g + theme(axis.text = element_text(face = "bold", size = 14),
                    axis.title = element_text(face = "bold", size = 14)))
      
      #Save these image to impress your supervisors again xD
      ggsave(paste0("Event No.", i, ".png"), height = 5, width = 8)
    }
  }
}







###### A function that get participant session name
get.participant_session_name = function(folder) {
  session = fromJSON(file = list.files(path = folder, pattern = "session.json", 
                                       recursive = TRUE, full.names = TRUE))
  
  participant_session_name = trimws(session$session$name)
  
  return(participant_session_name)
}



##### A function that drops the starting (1s) and ending (1s) data for each contraction - just want the flat lines, save each as a dataframe, then store in a list

get.contraction.df = function(change, mypoints, drop.start = 1000, drop.end = 1000) {
  
  raw.start.end.rectime = change$rectime[mypoints]
  
  contraction.data = list()
  for (i in 1:20) {
    contraction.data[[i]] = getsubdata(change, 
                                       start = raw.start.end.rectime[i*2 - 1] + drop.start, 
                                       stop = raw.start.end.rectime[i*2] - drop.end)
    
    print(myplot(contraction.data[[i]], name = paste0("Contraction No.", i)) + theme_classic())
  }
  
  ##### Mutate a new column `contration` number (from 1 to 20...) for each contraction data frame, then combine all back together
  
  for (i in 1:length(contraction.data)) {
    contraction.data[[i]] = contraction.data[[i]] %>% mutate(contraction = as.factor(i))
  }
  
  contraction.df = do.call("rbind", contraction.data)
  return(contraction.df)
  
}



##### A function that averages each contraction for each sensor, get the standard deviation as well
get.averaged.plot = function(contraction.df, save = FALSE) {
  
  avged.df = contraction.df %>% 
    tidy.pressure() %>% 
    group_by(contraction, sensor) %>% 
    summarise(mean = mean(pressure), sd = sd(pressure, na.rm = TRUE))
  
  
  participant_session_name = get.participant_session_name(folder)
  
  g = ggplot(data = avged.df, aes(x = contraction, y = mean, group = sensor, color = sensor)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    labs(title = "Mean Pressure over contraction", 
         x = "Contraction",
         y = "Mean pressure (mm Hg)", 
         subtitle = participant_session_name) +
    theme_classic() +
    scale_color_brewer(palette = "Set1")
  
  print(g)
  if (save == TRUE) {
    #Save these image to impress your supervisors again xD
    ggsave("Mean pressure for each sensor over contraction.png", width = 10, height = 5)
  }
}

















