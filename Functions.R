##### Preparing packages

# install.packages("tidyverse")
# install.packages("rjson")
# install.packages("quantmod")

library(tidyverse)
library(rjson)
library(quantmod)
library(pracma)
library(changepoint)
library(purrr)

###### A function that tidys up a given dataset
tidy = function(data){
  data %>%
    gather(sensor, pressure, p1:p8) %>%
    mutate(sensor = as.factor(sensor))
}

####### A function that return JSON information
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


###### A function to obtain all available exercise ids (as a list)
get_id_list = function(folder){
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

###### A function that obtain the description for a rough input exercise name (id)
get_description = function(id){
  # Choose a random(the first one) folder in the current wd
  folder = list.files(pattern = "_csv")[1]
  
  id_list = get_id_list(folder)
  if (is.null(id) == TRUE) {
     return(NULL)
  }
  # Get rid of info (.participantid) after "."
  if (length(strsplit(id, "\\.")[[1]]) > 1) {
    description = "Maximum PFM Contration for 5s."
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
  
  # Subtract the initial recording start timestamp to get the approximate time
  exercise_start_time = round((exercise_start - start) / 10) * 10
  exercise_stop_time = round((exercise_stop - start) / 10) * 10
  
  list(exercise_start_time, exercise_stop_time)
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




###### A function plot pressure values vs time (in seconds), 
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
    tidy() %>%
    ggplot(aes(x = time, y = pressure, color = sensor)) +
    geom_line(position = position_dodge(width = 0.2)) +
    scale_x_discrete(limits = seq(0, round(max(data$time)/1000) * 1000, gap), 
                     labels = seq(0, round(max(data$time)/1000), gap/1000)) +
    labs(x = "Time (s)", y = "Pressure (mmHg)",
         title = paste("Pressure trace from", name),
         caption = description)  +
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}


#### Not so useful but exist 
###### A function plot the pressure change vs rectime (in ms) (rectime is the original recording time variable in data)
plot2 = function(data, name = NULL){
  if (is.null(name) == TRUE) 
  {description = NULL} 
  else 
  {description = paste("Instruction:", get_description(name))}
  
  data %>% 
    tidy() %>%
    ggplot(aes(x = rectime, y = pressure, 
               group = sensor, color = sensor)) +
    geom_line(position = position_dodge(width = 0.2)) +
    labs(x = "Recording Time (ms)", y = "Pressure (mmHg)",
         title = paste("Pressure trace from", name),
         caption = description) +   
    scale_color_brewer(palette = "Set1") + 
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}


###### A function to remove baseline values

baseline = function(data, relaxdata){
  newdata = sweep(data[-1], 2, colMeans(relaxdata[-1]))
  newdata$rectime = data$rectime
  newdata
}


###### A run-everything process function - return the a list of 2 pfmc datasets (1 original, 1 change)
process = function(folder, whole = NULL){
  # Get the data
  whole = read.csv(list.files(path = folder, pattern = ".csv", recursive = TRUE, full.names = TRUE))
  
  # Get the json info of this subject
  json.info.list = get.json.info(folder)
  session = json.info.list$session
  profile = json.info.list$profile
  
  # Get participant id
  participant_id = json.info.list$session$patient_id
  
  # Rename columns
  colnames(whole) = c("rectime", paste0("p", 1:8), paste0("t", 1:8))
  
  data = whole %>% select(-(t1:t8))

  # Get the start/end time of the pfmc exercise
  name = "pfmc"
  rectime = getrectime(folder, name)
  
  # Get pfmc data
  pfmcdata = getsubdata(data, rectime[[1]], rectime[[2]])
  
  # Get the resting period - stable data
  attempt3 = getsubdata(data, rectime[[1]], rectime[[1]] + 15000)
  
  # Take out the baseline from data
  newdata = baseline(data, attempt3)
  
  # Subset the pfmc data
  pfmcchange = getsubdata(newdata, rectime[[1]], rectime[[2]])
  
  # Retutn the whole dataset, pfmc dataset, one original, one change
  datalist = list(data, newdata, pfmcdata, pfmcchange)
  
  names(datalist) = c(paste0("whole.participant", participant_id),
                      paste0("wholechange.participant", participant_id),
                      paste0("pfmcdata.participant", participant_id), 
                      paste0("pfmcchange.participant", participant_id))
  
  datalist
  
}



























