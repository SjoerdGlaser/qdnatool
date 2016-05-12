## Script to simulate data. Variables which are both calulated in the analysis 
## and are created in the simulation are name differently, to make sure the 
## analysis script is indepedent of the simulation results/input.

number.items = 25                       # Number of Questions
number.students = 50                    # Number of Students
number.answeroptions = rep(3 ,number.items) #Every item has 3 answer options
#number.answeroptions = sample(c(0,2:6), number.items, replace=TRUE) 
#Give each item a random max. of answer options.

# Define categories
category = sample(1:3, number.items, replace = TRUE, prob = c(.5,.4,.1)) 
#category = c(rep(1,number.items)) # No sub categories


key1 = numeric()    # Generate random correct answer option
for(i in 1:number.items) {
  if(number.answeroptions[i] == 0) { # Manually graded item
    key1[i] = 0
  } else {
    key1[i] = sample(1:number.answeroptions[i], 1)} #Pick one good answer option
}

key = matrix(0, max(number.answeroptions), number.items) #Create key matrix
for(i in 1: number.items)
  key[key1[i],i] = 1

if(nrow(key) == 0) # If all items are manually graded (no answer options).
  key = matrix(0, 1, number.items)

##Create extra right answer options
#key0 = matrix(sample(c(1,0), number.items * max(number.answeroptions), 
#                     replace=TRUE, prob=c(.1,.9)),
#              max(number.answeroptions), number.items) 
#key = key + key0
#key = ifelse(key > 1, 1, key) # 2 become 1.

# Make sure no answer option an item does not have is graded as correctly.
for(i in 1:number.items) {
  if(number.answeroptions[i] < max(number.answeroptions)) 
    key[(1 + number.answeroptions[i]) : max(number.answeroptions),i] = 0
}

#Generates given answers. Totally random
input.answers = matrix(0, number.students, number.items) 
for(j in 1:number.items) {
  if (number.answeroptions[j] == 0) { 
  	#If no answer options are given, either good (1) or false (0)
    input.answers[,j] = sample(0 : 1, number.students, TRUE)
  } else {
    input.answers[,j] = sample(1 : number.answeroptions[j], 
    													 number.students, TRUE)}
}

## Add missingness, now random 1% of the answers.
missing = matrix(sample(c(1,0), number.items * number.students, replace=TRUE, 
												prob=c(.99,.01)),
                 number.students, number.items)
input.answers = input.answers * missing

# Remove all variables not used in the analysis script.
rm(key0, missing, i, j, key1, number.items, number.students)

