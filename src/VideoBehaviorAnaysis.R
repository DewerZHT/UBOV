#####
# Author : Wu, Zhen-Hao
# LAST MODIFIED : 2017/06/24
# Desccription :
#   This is a R script for paper data analysis
###

#####
# Load Raw paper data 
print(paste0("Current working directory: ", getwd()))

# Load Student Homework 
print("Load Student Homework: Select Student HW 20170531_164953.csv")
homework = read.csv("Select Student HW 20170531_164953.csv")

# Load Student Video Behavior
print("Load Student Video Behavior: Select Student Behavior 20170531_212813.csv")
behavior1 = read.csv("Select Student Behavior 20170531_212813.csv")

# remove wrong video_id log
print("remove ")
behavior1 = behavior1[!behavior1$Video_id == 'YU4kS9gBAvY', ]
# cleaning NA value
behavior1 = na.omit(behavior1)
# cleaning empty video_id value
behavior1 = behavior1[!behavior1$Video_id == 'paused', ]

dim(homework)
dim(behavior1)


# Note that homework  11, 12, 13, 14, 15, 16 is class A
# 8, 9, 10, 17, 18, 19 is class B
# move specified homework into single dataframe
HW8 = homework[homework$Homework_id == '8',]   # Class B Lab06 1st
HW9 = homework[homework$Homework_id == '9',]   # Class B Lab06 2nd
HW10 = homework[homework$Homework_id == '10',] # Class B Lab06 3rd

# remove repeat student
HW10 = unique(HW10)

# move specified homework into single dataframe
HW11 = homework[homework$Homework_id == '11',] # Class A Lab06 1st
HW12 = homework[homework$Homework_id == '12',] # Class A Lab06 2nd
HW13 = homework[homework$Homework_id == '13',] # Class A Lab06 3rd

# remove repeat student
HW13 = unique(HW13)

# Note that homework 8, 9, 10, 14, 15, 16 is class A
# 11, 12, 13, 17, 18, 19 is class B
# move specified homework into single dataframe
HW14 = homework[homework$Homework_id == '14',] # Class A Lab07 1st
HW15 = homework[homework$Homework_id == '15',] # Class A Lab07 2nd
HW16 = homework[homework$Homework_id == '16',] # Class A Lab07 3rd

# remove repeat student
HW16 = unique(HW16)

# move specified homework into single dataframe
HW17 = homework[homework$Homework_id == '17',] # Class A Lab07 1st
HW18 = homework[homework$Homework_id == '18',] # Class A Lab07 2nd
HW19 = homework[homework$Homework_id == '19',] # Class A Lab07 3rd

# remove repeat student
HW19 = unique(HW19)

# check class A Lab06 3rd
print("class A Lab06 3rd Log")
head(HW13)
summary(HW13)

# check class B Lab06 3rd
print("class B Lab06 3rd Log")
head(HW10)
summary(HW10)

# check class A Lab07 3rd
print("class A Lab07 3rd Log")
head(HW16)
summary(HW16)

# check class B Lab07 3rd
print("class B Lab07 3rd Log")
head(HW19)
summary(HW19)

# 作業繳交應當畫圖
#   直方圖
#   HW Lab06 A
#   HW Lab06 B

#   HW Lab07 A
#   HW Lab07 B

videoList = behavior1$Video_id
videoList = unique(videoList)
# re-ordered videoList factor level (Note that)
videoList = factor(videoList)
videoList

studentList = behavior1$Account
typeof(studentList)
studentList = unique(studentList)

## example for 1st person in student list
# get first student
studentList[1]
# get this student's video behavior log index
studentLogIndex = which(behavior1$Account == studentList[1])
studentLogIndex
# subset dataframe with this student's video behavior log index
studentSubset = subset(behavior1, behavior1$Account == studentList[7])
studentSubset
# get student's play action from begin (loaded start)
which(studentSubset$Behavior == 'playing' & studentSubset$Video_Time == '0')
studentSubset.LogPausedIndex = which(studentSubset$Behavior == 'paused')

# get each student loaded video time 
c = 1
for( i in studentList ) {
  print(i)
  # subset dataframe with this student's video behavior log index
  studentSubset = subset(behavior1, behavior1$Account == studentList[c])
  # get student's play action from begin (loaded start)
  print(which(studentSubset$Behavior == 'playing' & studentSubset$Video_Time == '0'))
  c = c + 1
}

studentSubset = subset(behavior1, behavior1$Account == studentList[1])
sum(studentSubset$Video_Time) / 60

# get each student video watching time time 
c = 1
time_duration = c()
for( i in studentList ) {
  print(i)
  # subset dataframe with this student's video behavior log index
  studentSubset = subset(behavior1, behavior1$Account == studentList[c])
  # get student's play action from begin (loaded start)
  print(sum(studentSubset$Video_Time) / 60)
  time_duration[i] = sum(studentSubset$Video_Time) / 30
  c = c + 1
}

max(time_duration)
min(time_duration)
mean(time_duration)

which(time_duration == max(time_duration))
which(time_duration == min(time_duration))

studentSubset.LogPausedIndex[1]
studentSubset[studentSubset$Video_Time, studentSubset.LogPausedIndex[1]]

videoList = behavior1$Video_id
videoList = unique(videoList)
# re-ordered videoList factor level (Note that)
videoList = factor(videoList)
videoList

videoLogSubset.1st = subset(behavior1, behavior1$Video_id == 'gzCSV35U5-w') # 新增Express頁面與路徑
videoLogSubset.2nd = subset(behavior1, behavior1$Video_id == '6-Xh8cMtxao') # 
videoLogSubset.3rd = subset(behavior1, behavior1$Video_id == 'f-6ssLmmz64') #
videoLogSubset.4th = subset(behavior1, behavior1$Video_id == 'Cbm5zjFsHGs') # 
videoLogSubset.5th = subset(behavior1, behavior1$Video_id == 'TVfAery2QN0') # 網頁控制RaspberryPi的LED
videoLogSubset.6th = subset(behavior1, behavior1$Video_id == 'gF9QfR91O1Q') # 使用Express控制RaspberryPi上的LED
videoLogSubset.7th = subset(behavior1, behavior1$Video_id == 'b2-aWX5qKIQ') # 
videoLogSubset.8th = subset(behavior1, behavior1$Video_id == '1iqlpVQOO0M') # 
videoLogSubset.9th = subset(behavior1, behavior1$Video_id == 'syunlserp7s') # 使用Express(Node.js)控制伺服馬達

# 新增Express頁面與路徑
play_begin = videoLogSubset.1st$Video_Time == '0' & videoLogSubset.1st$Behavior == 'playing'
sum(play_begin, na.rm=TRUE)

time_count = table(videoLogSubset.1st$Video_Time)
time_count[1] = 0
time_count
which(time_count == max(time_count))

# 網頁控制RaspberryPi的LED
play_begin = videoLogSubset.5th$Video_Time == '0' & videoLogSubset.5th$Behavior == 'playing'
sum(play_begin, na.rm=TRUE)

time_count = table(videoLogSubset.5th$Video_Time)
time_count[1] = 0
time_count
max(time_count)
which(time_count == max(time_count))

# 使用Express控制RaspberryPi上的LED
play_begin = videoLogSubset.6th$Video_Time == '0' & videoLogSubset.6th$Behavior == 'playing'
sum(play_begin, na.rm=TRUE)

time_count = table(videoLogSubset.6th$Video_Time)
time_count[1] = 0
time_count
max(time_count)
which(time_count == max(time_count))

# 使用Express(Node.js)控制伺服馬達
play_begin = videoLogSubset.9th$Video_Time == '0' & videoLogSubset.9th$Behavior == 'playing'
sum(play_begin, na.rm=TRUE)

time_count = table(videoLogSubset.9th$Video_Time)
time_count[1] = 0
time_count
max(time_count)
which(time_count == max(time_count))

# 學生觀看總時間直方圖

# 學生觀看總次數直方圖

# 學生觀看重複次數直方圖

