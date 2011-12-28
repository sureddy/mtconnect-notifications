### MTConnect Condition and Notification Analysis ####
### (c) 2011, Manufacturing System Insights Inc., Berkeley CA


AlarmDataRead <- function(readpath, rawdata_format, machine_name, type)
# Function to Read the rawdata file according to the format
# Arguments
#     filepath = path of the file
#     rawdata_format = Format of the rawdata File 
#                         - "Vimana_MTC_events"
#                         - "Adapter"
#    type = Subtype of the alarm
    
# Output
#     rawdata      = Processed Rawdata file
#
{
    if(rawdata_format=="vimana_MTC_Events")
    {
        if(type == "alarm")
        {
            alarm_data_filename<-paste(readpath, machine_name, "_alarms.txt", sep="")
            rawdata<-read.table(alarm_data_filename, sep=",", header=T, stringsAsFactors=F)
        }
        if(type == "mode")
        {
            mode_data_filename <- paste(readpath, machine_name, "_mode.txt", sep="")
            rawdata<- read.table(mode_data_filename, sep=",", header=T, stringsAsFactors=F)
        }
        if(type == "exec")
        {
            execstatus_filename <- paste(readpath, machine_name, "_execution.txt", sep="")
            rawdata<- read.table(execstatus_filename, sep=",", header=T, stringsAsFactors=F)
            
        }
    }
    if(rawdata_format == "Adapter")
    {
         if(type == "alarm")
        {
            alarm_data_filename<-paste(readpath, machine_name, "_alarms.txt", sep="")
            rawdata<-read.table(alarm_data_filename, sep='|', header=T, stringsAsFactors=F)
        }
        if(type == "mode")
        {
            mode_data_filename <- paste(readpath, machine_name, "_mode.txt", sep="")
            rawdata<- read.table(mode_data_filename, sep='|', header=T, stringsAsFactors=F)
            rawdata <-rawdata[,c(1,3)]
        }
        if(type == "exec")
        {
            execstatus_filename <- paste(readpath, machine_name, "_execution.txt", sep="")
            rawdata<- read.table(execstatus_filename, sep='|', header=T, stringsAsFactors=F)
            rawdata <-rawdata[,c(1,3)]
            
        }
        rawdata[,1] = as.numeric(as.POSIXct(strptime(rawdata[,1], "%Y-%m-%dT%H:%M:%OSZ"), tz ="UTC"))
    }
    
    rawdata<- rawdata[order(rawdata[,1]),] # Arranging chronologically
    rownames(rawdata) = NULL
    rawdata[,1] = as.numeric(round(rawdata[,1],2)) # Rounding timestamps to integers to prevent floating errors
    return(rawdata)
}



CleanAndMerge <- function(data1, data2)
# Function which sets dataframes ready for merging and merges them. It 
#     1)removes superflous entries
#     2)removes identical timestamps
#     3)merges the data frames into a zoo object
# Arguments
#     data1, data2 = dataframes for merging
  
# Output
#     mergeddata      = merged zoo object
#
{
    #Remove superflous entries, 
    data1_n = data1
    for(i in 2:nrow(data1))
    {

        if(all(data1[i,2:ncol(data1)] == data1[(i-1),2:ncol(data1)]) )
        {
            data1_n[i,] = NA
        }
    }
    
    data1_n = na.omit(data1_n)
        
    data2_n = data2
    for(i in 2:nrow(data2))
    {
        if(all(data2[i,2:ncol(data2)] == (data2[(i-1),2:ncol(data2)])))
        {
            data2_n[i,] = NA
        }else
        if(data2[i, 1] == data2[(i-1), 1]) #Circumventing the conflict when Normal and Fault exist at the same point.
        {
            if(data2[i, 2] == error_state) data2_n[(i-1),] = NA
            if(data2[(i-1), 2] == error_state) data2_n[i,] = NA
        }
    }
    data2_n = na.omit(data2_n)
    
    data1_n[,1] = as.numeric(data1_n[,1])
    data2_n[,1] = as.numeric(data2_n[,1])
    x<-read.zoo(data1_n, index.column=1)
    y<-read.zoo(data2_n, index.column=1)
    mergeddata<-merge.zoo(x,y)

    mergeddata <- data.frame(mergeddata, stringsAsFactors=F)
    mergeddata = cbind(rownames(mergeddata), mergeddata)
    mergeddata<-AAFillNATimes(mergeddata) # Filling in NA times
    rownames(mergeddata) = NULL
    # Setting the index column as numerical, rest as Character
    mergeddata[,2:ncol(mergeddata)] = apply(mergeddata[,2:ncol(mergeddata)], 2, as.character)
    mergeddata[,1] = as.numeric(as.character(mergeddata[,1]))
    
    return(mergeddata)
}

AlarmStateStats <- function(parseddata, state_data, error_state)
#Calculates the State(ES/ CM) status of each alarm. Also gives the summary of each alarm type
# Arguments
#     parseddata = data for duration calculation
#     state_data = object containing the machine mode and execution status at each point of time
#     error_state = the alarm state which needs to be considered

# Output
#     alarm_state_status = List which contains the Ungrouped and Grouped alarm state status and also the summary of each alarm type.
#
{   
    alarm_state_status_summary = NULL # Summary of each alarm type
    unique_path<-unique(subset(parseddata, parseddata[,2]==error_state)[,3]) # Unique alarm paths
    alarm_state_status_full = alarm_state_status_grouped = NULL # CM/ES at each alarm path for unique alarms
    times = start_status = end_status =  end_status_grouped = times_grouped = start_status_grouped = NULL #Initializing
    
    for(k in 1:length(unique_path ))
    {
        pathsubset<-subset(parseddata, parseddata[,3]== unique_path[k])  # Subsetting the data into different unique paths 
        pathsubset<-data.frame(pathsubset,  stringsAsFactors=F)[,c(1,2,8)] # 1 - Timestamp, 2 - Alarm State, 8 - Alarm Text
        unique_alarms<-unique(subset(pathsubset[,3], pathsubset[,2] == error_state)) # Finding the unique alarms withing the path
        print(paste(k, "/", length(unique_path)," Unique Alarm Paths", sep = "")) # Counter which shows the current number of paths/total path

        for(j in 1:length(unique_alarms))
        {
            alarmsubset = subset(pathsubset, pathsubset[,2] == Normal_tag | pathsubset[,3] == unique_alarms[j])

            state_data = na.omit(state_data) 
            firing_count = AAFiringCount(alarmsubset, error_state) # Finding the number of firings for each alarm
            mergeddata <- AACleanandMerge(state_data, alarmsubset) # Merging the subset and the state data
            mergeddata<-na.omit(mergeddata) # Omitting any NA values in the beginning due to incomplete data.      
            
            flag = 0 # Flag which specifies if an alarm is active at the timestamp.
            alarm_stat = alarm_stat_grouped = NULL # Status of one singe alarm
            dur_mode = dur_exec = NULL # List containing the time spent by the alarm in each state
            current_exec = mergeddata[1, 2] ; current_mode = mergeddata[1, 3] # Current mode and execution status
            start = dur_mode$current_mode_index = dur_exec$current_exec_index =  1 # Index of start and current CM/ES
            time_to_next_alarm = c(NA, NA)
            f = 1 # Index for the firing_count
            
            for(i in 1:nrow(mergeddata))
            {
                if(mergeddata[i,5] == unique_alarms[j] & mergeddata[i,4] == error_state & flag == 0)
                    #if alarm name and error state matches and the alarm is not currently active
                {
                    flag = 1 # Alarm is now marked as active
                    start = dur_mode$current_mode_index = dur_exec$current_exec_index =  i # Index of start and current CM/ES same since alarm just started
                    current_mode = mergeddata[i, 3]; current_exec = mergeddata[i, 2] # Current mode and Exec status
                    dur_mode$AUTO = dur_mode$MAN = dur_mode$MDI = 0 # Duration of each mode
                    dur_exec$READY = dur_exec$ACTIVE = dur_exec$STOPPED = dur_exec$INTPD = 0 # Duration of each exec status
                    
                }
                if(flag != 0 & mergeddata[dur_mode$current_mode_index,3]!=mergeddata[i, 3] )
                #If alarm is active and current CM is different from previous CM, calculate the duration of the state
                {
                    dur_mode <- AADurMode(mergeddata, dur_mode, mergeddata[dur_mode$current_mode_index,3], i)
                }
                if(flag != 0 & mergeddata[dur_exec$current_exec_index,2]!=mergeddata[i, 2] )
                #If alarm is active and current ES is different from previous ES, calculate the duration of the state
                {
                    dur_exec <- AADurExec(mergeddata, dur_exec, mergeddata[dur_exec$current_exec_index,2], i)
                }
                                      
                if( (mergeddata[i,4] == Normal_tag & flag != 0) | (i == nrow(mergeddata) & flag != 0))
                # If alarm type = Normal, and alarm is active, or the last data point has been reached
                {
                    dur_mode <- AADurMode(mergeddata, dur_mode, current_mode, i) # Find CM duration in the corresponding state
                    dur_exec <- AADurExec(mergeddata, dur_exec, current_exec, i) # Find ES duration in the corresponding state

                    #Single alarm status includes total alarm duration and duration in each state
                    single_alarm_stat = cbind(firing_count[f], as.numeric(mergeddata[i,1])-as.numeric(mergeddata[start,1])
                                       , dur_mode$AUTO, dur_mode$MAN, dur_mode$MDI, dur_exec$READY, dur_exec$ACTIVE, dur_exec$STOPPED, dur_exec$INTPD)
                    f = f+1 # Alarm Firing count
                    
                    #Start and end of alarm in plant time
                    times = rbind( times, c(as.character(as.POSIXlt(as.double(mergeddata[start,1]), tz="UTC", origin="1970-01-01")), 
                                            as.character(as.POSIXlt(as.double(mergeddata[i,1]), tz="UTC", origin="1970-01-01")) ) )
                    start_status = rbind(start_status, c(mergeddata[start,2], mergeddata[start,3])) # CM/ES status at the start of the alarm
                    end_status = rbind(end_status, c(mergeddata[i,2], mergeddata[i,3])) # CM/ES status at the start of the alarm
                    alarm_stat = rbind(alarm_stat,single_alarm_stat) # Binding it into a dataframe
                    
                    # Grouped statuses
                    start_status_grouped = rbind(start_status_grouped, c(mergeddata[start,2], mergeddata[start,3])) 
                    end_status_grouped = rbind(end_status_grouped, c(mergeddata[i,2], mergeddata[i,3]))
                    alarm_stat_grouped = rbind(alarm_stat_grouped, single_alarm_stat)
                    times_grouped = rbind( times_grouped, c( as.character(as.POSIXlt(as.double(mergeddata[start,1]), tz="UTC", origin="1970-01-01")), 
                                            as.character(as.POSIXlt(as.double(mergeddata[i,1]), tz="UTC", origin="1970-01-01")) ) )

                    grouping_threshold = 3600 # Time between the end of one alarm and start of a similar alarm to be considered as a group
                    
                    if(nrow(alarm_stat_grouped)>1) # If there are multiple alarms of the same kind, check for grouping
                    {
                        if(as.numeric(difftime(as.POSIXct(times[nrow(times),1]),as.POSIXct(times[(nrow(times) - 1),2]), units='sec')) < grouping_threshold )
                        # IF time between end of one alarm and start of next alarm of the same kind is less than a threshold group the alarms
                        {
                            alarm_stat_grouped[(nrow(alarm_stat_grouped) -1), ] =  alarm_stat_grouped[(nrow(alarm_stat_grouped) -1), ] +
                                                                                    alarm_stat_grouped[nrow(alarm_stat_grouped), ]
                            alarm_stat_grouped = matrix(alarm_stat_grouped[1: (nrow(alarm_stat_grouped)-1), ], ncol=9)
                            times_grouped[(nrow(times_grouped)-1),2] = times_grouped[nrow(times_grouped),2]
                            times_grouped = times_grouped[1:(nrow(times_grouped)-1),]                      
                            start_status_grouped = start_status_grouped [1:(nrow(start_status_grouped)-1),]
                            end_status_grouped = end_status_grouped [1:(nrow(end_status_grouped)-1),]
                            
                        }
                    }
                    flag = 0 # Alarm marked as not active
                }
               
 
            }
        alarm_stat = round(alarm_stat,2)
        alarm_stat_grouped = round(alarm_stat_grouped,2)
        alarm_state_status_full = rbind(alarm_state_status_full, cbind(unique_alarms[j], unique_path[k], alarm_stat))  # All alarms of the kind, ungrouped
        alarm_state_status_grouped = rbind(alarm_state_status_grouped, cbind(unique_alarms[j],  unique_path[k],alarm_stat_grouped) )   # All alarms of the kind, ungrouped
        alarm_state_status_summary = rbind(alarm_state_status_summary, c(unique_alarms[j],  unique_path[k],apply(alarm_stat, 2, sum))) # Summary of alarms of the same kind
        }
    }
    #Finding the time difference between alarms
    if(nrow(times)>1)
    {
         time_to_next_alarm = c(as.numeric(difftime(times[2:nrow(times),1],times[1:(nrow(times)-1),2],  units='sec')), NA)
    }
    if(nrow(times_grouped)>1)
    {
        time_to_next_alarm_grouped = c(as.numeric(difftime(times_grouped[2:nrow(times_grouped),1],times_grouped[1:(nrow(times_grouped)-1),2], units='sec')), NA)
    }
    # Changin the wrongly assigned time diffs(different alarm texts) to NA
    for(l in 2:nrow(times))
    {
        if(alarm_state_status_full[l,1]!=alarm_state_status_full[(l-1),1] || alarm_state_status_full[l,2]!=alarm_state_status_full[(l-1),2]) time_to_next_alarm[l-1] = NA
    }
    for(l in 2:nrow(times_grouped))
    {
        if(alarm_state_status_grouped[l,1]!=alarm_state_status_grouped[(l-1),1] || alarm_state_status_grouped[l,2]!=alarm_state_status_grouped[(l-1),2]) time_to_next_alarm_grouped[l-1] = NA
    }

#     Binding into single dataframes
    alarm_state_status_full<-cbind(times, start_status, end_status, alarm_state_status_full, time_to_next_alarm) # Ungrouped alarm state status
    alarm_state_status_grouped<-cbind(times_grouped, start_status_grouped, end_status_grouped,  alarm_state_status_grouped, time_to_next_alarm_grouped) # Grouped status
    alarm_state_status_summary<-as.data.frame(alarm_state_status_summary, stringsAsFactors = F) # Summary of each alarm type
    rownames(alarm_state_status_grouped) = NULL
    colnames(alarm_state_status_summary) <-c("AlarmText", "AlarmPath", "AlarmCount", "DurationTotal",  "DurationModeAuto", "DurationModeManual", "DurationModeMDI", "DurationExecutionReady", "DurationExecutionActive", "DurationExecutionStopped", "DurationExecutionInterrupted")
    colnames(alarm_state_status_full)   = c("AlarmStart", "AlarmEnd", "ExecutionAtStart", "ModeAtStart","ExecutionAtEnd", "ModeAtEnd", "AlarmText","AlarmPath", "AlarmCount","DurationTotal", "DurationModeAuto", "DurationModeManual", "DurationModeMDI", "DurationExecutionReady", "DurationExecutionActive", "DurationExecutionStopped", "DurationExecutionInterrupted", "TimeToNextAlarm")
    colnames(alarm_state_status_grouped) = c("AlarmStart", "AlarmEnd", "ExecutionAtStart", "ModeAtStart","ExecutionAtEnd", "ModeAtEnd",  "AlarmText", "AlarmPath","AlarmCount","DurationTotal", "DurationModeAuto", "DurationModeManual", "DurationModeMDI", "DurationExecutionReady", "DurationExecutionActive", "DurationExecutionStopped", "DurationExecutionInterrupted","TimeToNextAlarm")
    alarm_state_status = list("ungrouped" = as.data.frame(alarm_state_status_full, stringsAsFactors=F), "grouped" = as.data.frame(alarm_state_status_grouped, stringsAsFactors=F), "summary" = alarm_state_status_summary)
    
    return(alarm_state_status)
    
}
                    
    
AlarmStateTransition <- function(Alungrouped, state_data)
# Function that shows the meta-state transitions in each alarm separately as a string
# Arguments
#     state_data = object containing the machine mode and execution status at each point of time
#     Alungrouped = Ungrouped alarm data

# Output
#     fullstring = The string that contains the ES/CM status of each alarm
#    
{
    fullstring = NULL
    for(i in 1:nrow(Alungrouped))
    {
        string = NULL
        start_time = as.numeric(as.POSIXct(Alungrouped[i,1], origin="1970-01-01", tz = "UTC")) #Start time in UTC
        
        end_time = as.numeric(as.POSIXct(Alungrouped[i,2], origin="1970-01-01", tz = "UTC")) # End time in UTC
        rowno_start = tail(which(as.numeric(state_data[,1]) <= start_time  ), 1) # starting rowno of the current alarm group
        rowno_end = tail(which(as.numeric(state_data[,1]) <= end_time ),1) # Ending rowno of the current alarm group
        
        start = start_time # Iterated starting time(equal to start time in the beginning)
        if(rowno_start!=nrow(state_data))
        {
        end = as.numeric(state_data[(rowno_start+1),1]) # Iterated end time (equal to the start time of the next row value)
            for(j in rowno_start:rowno_end)
            {
                if(end_time < end ) # If the iterated end time is more than the actual end time of the alarm, the iterated end time is changed to alarm end time
                {
                    end = end_time
                }
                if(end>=start & j+2<=nrow(state_data)) # If end < start and last row hasn't been reached
                {
                    #Alarm string updation
                    string = paste(string, state_data[j, 2], "/", state_data[j, 3], ": ", end - start,  " ", sep = "", collapse="")
                    start = end # Start becomes the start of the next string
                    end = as.numeric(state_data[(j+2),1]) # End becomes the start of the string subsequent to that
                }
            }
        } else
        {
            j = rowno_start
            string = paste(state_data[rowno_start, 2], "/", state_data[rowno_start, 3], ": ", end_time-start_time,  " ", sep = "", collapse="")
        }
        fullstring = rbind(fullstring, string) # Each completed string is bound to a data frame
    }
    rownames(fullstring) = NULL
    colnames(fullstring) = "Change in CM/ES Status:Duration in Secs"
    return(fullstring)
}

    
AlarmStateTransitionRace <- function(Algrouped, state_data, lead_window = 10, lag_window = 10)
# Function that shows the meta-state transitions in the neightbourhgood of start of each alarm separately as a string
# Arguments
#     state_data = object containing the machine mode and execution status at each point of time
#     Alungrouped = Ungrouped alarm data

# Output
#     fullstring = The string that contains the ES/CM status of each alarm
#    
{
    fullstring = NULL 
    for(i in 1:nrow(Algrouped))
    {
        racestring = NULL
        start_time = as.numeric(as.POSIXct(Algrouped[i,1], origin="1970-01-01", tz = "UTC")) - lead_window #Start time in UTC
        
        end_time = as.numeric(as.POSIXct(Algrouped[i,1], origin="1970-01-01", tz = "UTC"))  + lag_window # End time in UTC
        rowno_start = tail(which(as.numeric(state_data[,1]) <= start_time), 1) # starting rowno of the current alarm group
        rowno_end = tail(which(as.numeric(state_data[,1]) <= end_time ),1) # Ending rowno of the current alarm group
        
        window = state_data[rowno_start:rowno_end,]
        window[,1] = round(window[,1] - (start_time + lead_window),2)
        string = paste(as.character(t(window)), collapse=" ")
        fullstring = rbind(fullstring, string) # Each completed string is bound to a data frame
    }
    rownames(fullstring) = NULL
    colnames(fullstring) = "Change in CM/ES Status around the Alarm"
    return(fullstring)
}
                

AlarmGroupedStateTransition <- function(Alungrouped, Algrouped, state_change_string)
# Function that shows the takes in the Ungrouped and Grouped meta-state transitions in each alarm group as a string
# Arguments
#     Algrouped = Grouped alarm data
#     state_data = object containing the machine mode and execution status at each point of time
#     Alungrouped = Ungrouped alarm data

# Output
#     grouped_string = ES/CM status of each alarm group
#
{
    grouped_string = rep(NA, nrow(Algrouped))
    for(i in 1:nrow(Algrouped))
    {
        start_row = head(which(Alungrouped[,1] == Algrouped[i,1] & Alungrouped[,7] == Algrouped[i,7]),1)
        end_row =  head(which(Alungrouped[,2] == Algrouped[i,2] & Alungrouped[,7] == Algrouped[i,7]),1)
        grouped_string[i] = state_change_string[start_row]
        
        while(start_row<end_row)
        {
            timediff = as.numeric(difftime(Alungrouped[(start_row+1),1], Alungrouped[ start_row, 2], units='sec'))
            grouped_string[i] = paste(grouped_string[i], "NORMAL: ", timediff," ", state_change_string[start_row+1], sep = "")
            start_row = start_row + 1
        }
    }
    return(matrix(grouped_string, ncol=1))
}
            
            
        
               
AlarmModeDuration<- function(mergeddata, dur_mode, current_mode, i)
#Calculates time spent by the Alarm in the current mode and updates the dur_mode string. Called by the AAStateTransition Function
{
    if(current_mode == "AUTOMATIC")
    {
        dur_mode$AUTO = dur_mode$AUTO + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_mode$current_mode_index,1])
    }
    if(current_mode == "MANUAL")
    {
        dur_mode$MAN = dur_mode$MAN + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_mode$current_mode_index,1])
    }
    if(current_mode == "MANUAL_DATA_INPUT")
    {
        dur_mode$MDI = dur_mode$MDI + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_mode$current_mode_index,1])
    }
    dur_mode$current_mode_index = i
    return(dur_mode)
}

AlarmExecutionDuration <- function(mergeddata, dur_exec, current_exec, i)
#Calculates time spent by the Alarm in the current Execution Status and updates the dur_mode string. Called by the AAStateTransition Function
{
    if(current_exec == "READY")
    {
        dur_exec$READY = dur_exec$READY + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_exec$current_exec_index,1])
    }
    if(current_exec == "ACTIVE") 
    {
        dur_exec$ACTIVE = dur_exec$ACTIVE + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_exec$current_exec_index,1])
    }
    if(current_exec == "STOPPED") 
    {
        dur_exec$STOPPED = dur_exec$STOPPED + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_exec$current_exec_index,1])
    }
    if(current_exec == "INTERRUPTED") 
    {
        dur_exec$INTPD = dur_exec$INTPD + as.numeric(mergeddata[i,1]) - as.numeric(mergeddata[dur_exec$current_exec_index,1])
    }
    dur_exec$current_exec_index = i
    return(dur_exec)
}
    
AlarmFillNAs <- function(data)
# Function to fill the times with NA values with the value of the Alarm preceding it. Also removes the machine name from the columnnames for brevity
# Arguments
#     data = data for filling the NA times

# Output
#     data = Data frame with NA times filled by the previous Non NA Value
#
{
    data<-as.matrix(data)
    
    for(i in 1:ncol(data))
    {
        for(j in 2:nrow(data))
        {
            val = data[(j-1),i]
            if(is.na(data[j,i]))
            {
                data[j,i] = val
            }
        }
    }
    data<-as.data.frame(data, stringsAsFactors=F)
    return(data)
}

AlarmStringToPercentage <- function(state_change_string_grouped)
#Calculates the State(ES/ CM) status of each alarm as as percentage and outputs the string as a (HUGE!)table
# Arguments
#     state_change_string_grouped = The string which contains the Details about each alarm meta-state
# Output
#     transition_table = Table which contains the alarm transitions grouped as a table with Alarm meta-state, percentage and time in seconds in successive columns
{
    temp = NULL
    for(i in 1:nrow(state_change_string_grouped))
    {
        temp[i] = length(unlist(strsplit(state_change_string_grouped[i,1], split=" ")))
    }# Finding the maximum string length and multiplying with 3/2 to find the number of columns
    transition_table = matrix(NA, nrow=nrow(state_change_string_grouped), ncol=max(temp)/2*3) 
    total_dur_metastate = NULL
    for(i in 1:nrow(state_change_string_grouped))
    {
     
        alarm_transition_matrix<-matrix(unlist(strsplit(state_change_string_grouped[i,1], split=" ")), ncol=2, byrow=T)
        transition_percentage = round(as.numeric(alarm_transition_matrix[,2])*100/ sum(as.numeric(alarm_transition_matrix[,2])),2)
        alarm_transition_matrix = cbind(alarm_transition_matrix, transition_percentage)
        alarm_transition_vector = as.character(t(alarm_transition_matrix))
        total_dur_metastate = rbind(total_dur_metastate, AAMetastateTotalDur(alarm_transition_vector, 2))
        transition_table[i,1:length(alarm_transition_vector)] = alarm_transition_vector
    }
    return(data.frame(total_dur_metastate, transition_table))
}


AlarmMetaStateSummary <- function(state_change_table_ungrouped)
    #Calculates the State(ES/ CM) status of each alarm as as percentage and outputs the string as a (HUGE!)table
# Arguments
#     state_change_string_grouped = The string which contains the Details about each alarm meta-state
# Output
#     transition_table = Table which contains the alarm transitions grouped as a table with Alarm meta-state, percentage and time in seconds in successive columns

{
    unique_paths <- unique(state_change_table_ungrouped$AlarmPath)
    state_summary_full = NULL
    relevant_table = state_change_table_ungrouped[,c(7:8, 17:28)]
    for(k in 1:length(unique_paths))
    {
        pathsubset = subset(relevant_table, relevant_table$AlarmPath == unique_paths[k])
        unique_alarms <- unique(pathsubset$AlarmText)
        for(i in 1:length(unique_alarms))
        {
            subset_state = subset(pathsubset, pathsubset$AlarmText == unique_alarms[i])
            state_summary = round(apply(subset_state[,3:14], 2, sum),2)
            state_summary_full = rbind(state_summary_full, state_summary)
        }
    }
    rownames(state_summary_full)  = NULL
    return(state_summary_full)    
}
    

AlarmMetaStateTotalDuration <- function(alarm_transition_vector, k)
{
#Calculates the Total Duration of each metastate in the vector.
#K is the index which signifies the offset of the summed variable. k = 2 for duration, k= 3 for percentages

    dur_meta=NULL
    dur_meta$AA = dur_meta$RA = dur_meta$SA = dur_meta$IA = 0
    dur_meta$AM = dur_meta$RM = dur_meta$SM = dur_meta$IM = 0
    dur_meta$AD = dur_meta$RD = dur_meta$SD = dur_meta$ID = 0
    current_metastate = alarm_transition_vector[1]
    for(i in 1:(length(alarm_transition_vector)/3))
    {
        current_metastate = alarm_transition_vector[(3*(i-1))+1]
        if(current_metastate == "READY/AUTOMATIC:")
        {
            dur_meta$RA = dur_meta$RA + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "ACTIVE/AUTOMATIC:") 
        {
            dur_meta$AA = dur_meta$AA + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "STOPPED/AUTOMATIC:") 
        {
            dur_meta$SA = dur_meta$SA + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "INTERRUPTED/AUTOMATIC:") 
        {
            dur_meta$IA = dur_meta$IA + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "READY/MANUAL_DATA_INPUT:")
        {
            dur_meta$RD = dur_meta$RD + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "ACTIVE/MANUAL_DATA_INPUT:") 
        {
            dur_meta$AD = dur_meta$AD + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "STOPPED/MANUAL_DATA_INPUT:") 
        {
            dur_meta$SD = dur_meta$SD + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "INTERRUPTED/MANUAL_DATA_INPUT:") 
        {
           dur_meta$ID = dur_meta$ID + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "READY/MANUAL:")
        {
            dur_meta$RM = dur_meta$RM + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "ACTIVE/MANUAL:") 
        {
            dur_meta$AM = dur_meta$AM + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "STOPPED/MANUAL:") 
        {
            dur_meta$SM = dur_meta$SM + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
        if(current_metastate == "INTERRUPTED/MANUAL:") 
        {
            dur_meta$IM = dur_meta$IM + as.numeric(alarm_transition_vector[(3*(i-1))+k])
        }
    }
    return(as.data.frame(dur_meta))
}

AlarmFiringCount <-function(alarmsubset, error_state)
#Calculates the number of firings of each alarm by looping through the dataframe and finding repeats before alarm close
{
    firing_count_array = NULL
    firing_count = 0
    
    for(i in 1:nrow(alarmsubset))
    {
        if(alarmsubset[i,2]==error_state & firing_count == 0)
        {
            firing_count = 1
        }else
        if(alarmsubset[i,2]==error_state & firing_count >0)
        {
            firing_count = firing_count + 1
        }
        if(alarmsubset[i,2]== Normal_tag  & firing_count >= 1)
        {
            firing_count_array = append(firing_count_array, firing_count)
            firing_count = 0
        }
    }
    return(firing_count_array)
}



AlarmDuration <- function(parseddata, error_state)
#Calculates the Duration of each alarm - OBSE
# Arguments
#     parseddata = data for duration calculation
#     error_state = the alarm state which needs to be considered
    

# Output
#     alarm_active_dur = Duration of each alarm
#
{    
    alarm_active_dur = NULL 
    data_sub <- subset(parseddata, parseddata[,2]==error_state) # Subsetting the data into only the checked error state
    unique_path<-unique(subset(parseddata, parseddata[,2]==error_state)[,3]) # Unique paths
#     data_sub = apply(data_sub(1,3,4)], 2, as.character) # Converting it into character for companrison
    for(k in 1:length(unique_path ))
    {
        pathsubset<-subset(parseddata, parseddata[,3]== unique_path[k])  # Subsetting the data into different unique paths 
#         pathsubset<-data.frame(pathsubset,  stringsAsFactors=F)
        warn_alarm_stat = NULL # data item containing the alarm statistics
        flag = 0 # Flag to check if the alarm state is active
        i = 1
        while(i < nrow(pathsubset))
        {
            if(pathsubset[i,2]==error_state & flag == 0)
            {
                start = i
                flag = 1
            }else
            if(pathsubset[i,2]==error_state & flag != 0)
            {
                flag = flag + 1
            }
            
            if(pathsubset[i,2]== Normal_tag  & flag != 0)
            {
                warn_alarm_stat = rbind(warn_alarm_stat, 
                                         c(as.numeric(pathsubset[i,1])-as.numeric(pathsubset[start,1]),
                                         flag, pathsubset[start,ncol(pathsubset)],
                                         as.character(as.POSIXlt(as.double(pathsubset[start,1]), tz="UTC", origin="1970-01-01")),
                                         as.character(as.POSIXlt(as.double(pathsubset[i,1]), tz="UTC", origin="1970-01-01")), 
                                         (pathsubset[start,3:6]), NA, NA)) 
                flag = 0
                if(nrow(warn_alarm_stat)>1)
                {
                    warn_alarm_stat[(nrow(warn_alarm_stat)-1), 11] = as.numeric(as.POSIXct(as.character(warn_alarm_stat[nrow(warn_alarm_stat), 4]),tz="UTC")) -
                                                                 as.numeric(as.POSIXct(as.character(warn_alarm_stat[(nrow(warn_alarm_stat)-1), 5]), tz="UTC"))
                    warn_alarm_stat[(nrow(warn_alarm_stat)-1), 10] = as.numeric(warn_alarm_stat[(nrow(warn_alarm_stat)-1), 11]) + 
                                                                    as.numeric(warn_alarm_stat[(nrow(warn_alarm_stat)-1), 1])/2 + 
                                                                    as.numeric(warn_alarm_stat[nrow(warn_alarm_stat), 1])/2
                }
                    
            }
            i = i+1
        }
        alarm_active_dur<-rbind(alarm_active_dur, warn_alarm_stat)
    }
    colnames(alarm_active_dur) = c("durAlarm", "firingsCount", "alarmText", "startTime", "endTime", "Path", "sampleType", "nativeCode", "qualifier", "Time to next alarm", "Time to next alarm(mid-mid)")

    return(alarm_active_dur)
}
