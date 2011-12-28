### MTConnect Condition and Notification Analysis ####
### (c) 2011, Manufacturing System Insights Inc., Berkeley CA

# Load require R libraries
library("zoo")
library("ggplot2")
library("plyr")

# Set functions source
source("fnsAlarmAnalysis.R")

# Set data format
# "Adapter": MTConnect Adapter Logs
# "vimana_MTC_Events": vimana MTCEvents collection

rawdata_format = "vimana_MTC_Events"
Normal_tag <<- "Normal" # Define how "normal" states are tagged
ConditionType = "Fault" # Condition Type to be analysd

# Set data file paths
readpath <- "../data/" # Read Path
writepath = "../results/" # Write path

# Set device name
device_name = "sample"

# Reading Alarm, Execution, and Mode data
# The file names are in the format: extract_{alarm|execution|mode}_{device_name}.txt
alarmdata <- AlarmDataRead(readpath, rawdata_format, device_name, "alarm")
execdata <- AlarmDataRead(readpath, rawdata_format, device_name, "exec")
modedata <- AlarmDataRead(readpath, rawdata_format, device_name, "mode")

# Merge Execution and Mode data into a single dataframe and delete superfluous entries.
state_data <- CleanAndMerge(execdata, modedata)

# Arranging the data into a standard format for analyis
parseddata <- cbind(alarmdata)

#, NA)
# parseddata <- parseddata[,c(1,3,2,4:6, 8, 7)] 
# colnames(parseddata) = c("timestamp", "type","path","Dummy-sampleType", "nativeCode",  "nativeSeverity", "qualifier",  "alarm_text")

# Simple statistics -- without State Analysis
# alarm_simple_stats <- AlarmDuration(parseddata, ConditionType)

# Main Alarm Statistics Function
alarm_state_stats <- AlarmStateStats(parseddata, state_data, ConditionType)
AlarmsUnGrouped <- alarm_state_stats$ungrouped #Alarm statistics without grouping alarms that are close-by
AlarmsGrouped <- alarm_state_stats$grouped #Alarm statistics with clumped alarms considered as a single one
AlarmSummary <- alarm_state_stats$summary #Summary of each type of alarm Separately
    
# Find the meta-state transition of each alarm as a string
state_change_string_ungrouped <- AlarmStateTransition(AlarmsUnGrouped, state_data)

# Find the meta-state transition of each alarmgroup as a string, taking into account race-conditions
state_change_string_grouped <- AlarmGroupedStateTransition(AlarmsUnGrouped, AlarmsGrouped, state_change_string_ungrouped)
state_change_near_alarm_ungrouped <- AlarmStateTransitionRace(AlarmsUnGrouped, state_data, 10, 10)
state_change_near_alarm_grouped <- AlarmStateTransitionRace(AlarmsGrouped, state_data, 10, 10)
    
# Bind the Alarm status and the state change
AlarmsUnGrouped <- data.frame(AlarmsUnGrouped, StringToPercentage(state_change_string_ungrouped)[,1:12],state_change_string_ungrouped, state_change_near_alarm_ungrouped, stringsAsFactors=F)
AlarmsGrouped <- data.frame(AlarmsGrouped, StringToPercentage(state_change_string_grouped)[,1:12], state_change_string_grouped, state_change_near_alarm_grouped, stringsAsFactors=F)
    
# Convert the state change string to the percentage values and to a table with each row representing one alarm group
state_change_table_grouped <- cbind(AlarmsGrouped[,1:16], StringToPercentage(state_change_string_grouped))
state_change_table_ungrouped <- cbind(AlarmsUnGrouped[,1:16], StringToPercentage(state_change_string_ungrouped))
    
# Find summary of the Metastates for each type of Alarm
state_change_summary <- cbind(AlarmSummary, AlarmMetaStateSummary(state_change_table_ungrouped))
colnames(state_change_summary)[12:23] = c("Intpd-Auto", "Stopped-Auto", "Ready-Auto", "Active-Auto", "Intpd-Manual", "Stopped-Manual", "Ready-Manual", "Active-Manual", "Intpd-MDI", "Stopped-MDI", "Ready-MDI", "Active-MDI")

# Write PNG Plots
p1 = qplot(data=AlarmSummary, AlarmText, as.numeric(as.character(AlarmCount)), geom="bar") + coord_flip() + opts(title="Alarm Count") + scale_x_discrete("Alarm Text") + scale_y_continuous("Alarm Count")    
p2 = qplot(data=AlarmSummary, AlarmText, as.numeric(as.character(DurationTotal)), geom="bar") + coord_flip() + opts(title="Alarm Duration") + scale_x_discrete("Alarm Text") + scale_y_continuous("Alarm Duration (seconds)")    

AlarmsByMode <- melt(cbind(AlarmSummary[1], AlarmSummary[5:7]), "AlarmText")
AlarmsByMode$value = as.numeric(as.character(AlarmsByMode$value))
p3 = qplot(data=AlarmsByMode, AlarmText, value, fill=variable, geom="bar") + coord_flip() + opts(title="Alarm Duration by ControllerMode") + scale_y_continuous("Duration (seconds)") + scale_x_discrete("Alarm Text")

AlarmsByExec <- melt(cbind(AlarmSummary[1], AlarmSummary[8:11]), "AlarmText")
AlarmsByExec$value = as.numeric(as.character(AlarmsByExec$value))
p4 = qplot(data=AlarmsByExec, AlarmText, value, fill=variable, geom="bar") + coord_flip() + opts(title="Alarm Duration by ExecutionStatus") + scale_y_continuous("Duration (seconds)") + scale_x_discrete("Alarm Text")

AlarmsByState = melt(cbind(state_change_summary[1], state_change_summary[12:23]), "AlarmText")
AlarmsByState$value = as.numeric(as.character(AlarmsByState$value))
p5 = qplot(data=AlarmsByState, AlarmText, value, fill=variable, geom="bar") + coord_flip() + opts(title="Alarm Duration by State") + scale_y_continuous("Duration (seconds)") + scale_x_discrete("Alarm Text")

ggsave(paste(writepath, "AlarmCount.png", sep=""), p1)
ggsave(paste(writepath,"AlarmDuration.png", sep=""), p2)
ggsave(paste(writepath,"AlarmDurationByMode.png", sep=""), p3)
ggsave(paste(writepath,"AlarmDurationByExec.png", sep=""), p4)
ggsave(paste(writepath,"AlarmDurationByState.png", sep=""), p5)

# Write Outputs to CSV
write.csv(AlarmsUnGrouped, paste(writepath, device_name,  ConditionType, "_alarm_state_stats_ungrouped.csv", sep="", collapse=""))
write.csv(state_change_summary, paste(writepath, device_name,  ConditionType, "_alarm_state_stats_summary.csv", sep="", collapse=""))
write.csv(state_change_table_ungrouped, paste(writepath, device_name,  ConditionType, "_alarms_metastate_table_ungrouped.csv", sep="", collapse=""))
write.csv(state_change_table_grouped, paste(writepath, device_name,  ConditionType, "_alarms_metastate_table_ungrouped.csv", sep="", collapse=""))
