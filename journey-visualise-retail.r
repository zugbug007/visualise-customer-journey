# library(readr)
# library(plotly)
# library(RColorBrewer)
# 
# channel_stacks <- read_csv("data/data-shop.csv", skip = 11)
# colnames(channel_stacks) <- c("path", "conversion","path_count", "conversion_rate")
# head(channel_stacks)
# 
# s1 = plot_ly(
#   channel_stacks, 
#   y=~conversion, 
#   x=~path_count,
#   color=~conversion_rate, 
#   size=~conversion_rate,
#   text=~path
# ) %>% layout(
#   xaxis = list(type="log", title="Number of Paths"),
#   yaxis = list(type="log", title="Number of Conversions")
# ) %>% colorbar(
#   title = "Rate"
# )
# 

library(readr)
library(plotly)
library(RColorBrewer)
# Install adobeanayticsr from github
#devtools::install_github('benrwoodard/adobeanalyticsr', force = FALSE) 

library(adobeanalyticsr)
#Test Token has been refreshed and is upto date.
aw_token()
#delete the aa.auth file in WD if issues

#Set Date range for Adobe Data pull below
#Last 90 days starting from yesterday
date_range = c(Sys.Date() - 10, Sys.Date() - 1)

# Setup for Adobe Data
aw_metrics <- aw_get_metrics()
aw_dims <- aw_get_dimensions()
aw_reportsuites <- aw_get_reportsuites()
aw_calculatedmetrics <- aw_get_calculatedmetrics()
aw_segments <- aw_get_segments()

#Get the Adobe channel manager data using the 2.0 API from adobeanayticsr library
channel_stack_adobe <- aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = date_range,
  dimensions = c("evar38"),
  metrics = c("event182","evar38instances"),
  top = c(50000),
  page = 0,
  filterType = "breakdown",
  segmentId = NA, 
  metricSort = "desc",
  include_unspecified = FALSE,
  search = NA,
  prettynames = FALSE,
  debug = FALSE
)

#load CSV
#channel_stacks <- read_csv("data/data-membership.csv", skip = 11)
channel_stacks <- channel_stack_adobe
colnames(channel_stacks) <- c("path", "conversion","path_count")
head(channel_stacks)
# Plot the scatter if needed (off for now)
# p1 = plot_ly(
#   channel_stacks, 
#   y=~conversion, 
#   x=~path_count,
#   color=~conversion_rate, 
#   size=~conversion_rate,
#   text=~path
# ) %>% layout(
#   xaxis = list(type="log", title="Number of Paths"),
#   yaxis = list(type="log", title="Number of Conversions")
# ) %>% colorbar(
#   title = "Rate"
# )
# p1

s1
channel_stacks$path_list = strsplit(x=channel_stacks$path,split=">")
depth = 4

#Generate node labels and label length vectors
node_labels=rep(list(list()),depth)
label_length = list()
for(i in 1:depth){
  for(j in 1:length(channel_stacks$path)){
    if(!is.na(channel_stacks$path_list[j][[1]][i]))
      node_labels[[i]][j] = channel_stacks$path_list[j][[1]][i]
  }
  node_labels[[i]] = unique(unlist(node_labels[[i]]))
  node_labels[[i]] = node_labels[[i]][order(node_labels[[i]])]
  label_length[[i]] = length(node_labels[[i]])
}
node_labels = unlist(node_labels)
label_length = unlist(label_length)

#Build a data frame to fill out with each path view
combos = NULL
for(i in 1:(depth-1)){
  for(j in (1 + sum(label_length[1:i-1])):(label_length[i] + sum(label_length[1:i-1]))){
    for(k in (1 + label_length[i] + sum(label_length[1:i-1])):(label_length[i+1] + label_length[i] + sum(label_length[1:i-1]))){
      combos = rbind(combos, c(i,j,k,0))
    } 
  }
}
combos = as.data.frame(combos)
names(combos) = c("step","source","target","value")


#Populate the combo table
for(i in 1:(dim(combos)[1])){
  for(j in 1:(dim(channel_stacks)[1])){
    combos$value[i] = sum(combos$value[i], ifelse(
      (node_labels[combos$source[i]] == channel_stacks$path_list[j][[1]][combos$step[i]]) &
        (node_labels[combos$target[i]] == channel_stacks$path_list[j][[1]][combos$step[i]+1]),
      channel_stacks$path_count[j],0), na.rm = TRUE)
  }
}
#OK to here
#Add a node to populate with conversion values
uniques = unique(c(combos$source,combos$target))

converts = as.data.frame(list("step"=rep(0,length(uniques)), "source"=uniques, "target"=rep(max(uniques)+1,length(uniques)), 
                              "value"=rep(0,length(uniques))))
combos = rbind(combos,converts)
for(i in 1:(dim(channel_stacks)[1])){
  stack_depth = min(depth,length(channel_stacks$path_list[i][[1]]))
  index_val = which(combos$step==0 & combos$source==(which(node_labels == channel_stacks$path_list[i][[1]][stack_depth]) + 
                                                       ifelse(stack_depth>1, sum(label_length[1:(stack_depth-1)]),0)))
  combos$value[index_val] = combos$value[index_val] + channel_stacks$conversion[i]
}
#Populate the conversion node values
display_node_labels = node_labels
for(i in 1:length(label_length)){
  for(j in 1:label_length[i]){
    display_node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))] = paste0(i,":",node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))])
  }
}
display_node_labels = c(display_node_labels, "Conversion")
#Prep the Sankey Title based on Date and Channel
sankey_title <- paste("Membership Channel Conversion Flow - Shop Sales ", date_range[1], "-", date_range[2])

#Generate Sankey diagram
s2 <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = display_node_labels,
   #color = node_colors,
    pad = 10,
   alpha = 0.9,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.1
    )
  ),
  
  link = list(
    source = combos$source-1, # convert to zero index
    target = combos$target-1, # convert to zero index
    value = combos$value, #size of connection
    color = 'rgba(0,0,0,0.1)'
    #color = combos$color #add colors for each link if desired
  )
) %>% 
  layout(
    title = sankey_title,
    font = list(
      size = 10
    )
  )
s2

