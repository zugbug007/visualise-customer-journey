library(readr)
library(plotly)
library(RColorBrewer)

channel_stacks <- read_csv("data/data-shop.csv", skip = 11)
colnames(channel_stacks) <- c("path", "conversion","path_count", "conversion_rate")
head(channel_stacks)

s1 = plot_ly(
  channel_stacks, 
  y=~conversion, 
  x=~path_count,
  color=~conversion_rate, 
  size=~conversion_rate,
  text=~path
) %>% layout(
  xaxis = list(type="log", title="Number of Paths"),
  yaxis = list(type="log", title="Number of Conversions")
) %>% colorbar(
  title = "Rate"
)
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

#Generate Sankey diagram
s2 <- plot_ly(
  type = "sankey",
  orientation = "v",
  
  node = list(
    label = display_node_labels,
   #color = node_colors,
    pad = 10,
   alpha = 0.1,
    thickness = 30,
    line = list(
      color = "black",
      width = 0
    )
  ),
  
  link = list(
    source = combos$source-1, # convert to zero index
    target = combos$target-1, # convert to zero index
    value = combos$value #size of connection
    #color = combos$color #add colors for each link if desired
  )
) %>% 
  layout(
    title = "Channel Conversion Flow - Shop Sales - Oct 1st - Dec 13th 2020",
    font = list(
      size = 10
    )
  )
s2

