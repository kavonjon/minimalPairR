# Hi! Feel free to run first and ask questions later!
# That's how this code is designed.
# (You might need to install some packages though! See below for that.)


########### ... these comments declare sections of codes
## ... these comments explain code where you can specify inputs. If code preceding these comments is commented out, you will be prompted for the inputs when the code runs
# ... these are general comments explaining the code
#... these are variables that can be set when the comment is removed

########### Introduction
# this script takes a word list as input
# and outputs minimal sets and minimal pairs w/ the differing characters identified
#
# this code is presented as a script, rather than a function, in order to make it
# easier to understand and tweak for beginning coders. Inputs can be specified in the inputs
# section of the script. If left unspecified, users will be prompted for the inputs
# when the script runs. This code and commentary is designed to be clear and straightfoward,
# but basic knowledge of R is assumed.
# INPUTS: 1) a .csv, .ods, or .xlsx file. Each of the words of the wordlist
#     should be on its own row, and the word list should be in one column of the sheet
#         2) the name of the sheet where the wordlist is located
#         3) the indeces of the cell where the worldlist begins (not including header rows)
#         4) optionally: A list of digraphs, as a character vector
#         5) optionally: if the wordlist is case-sensitive
#                (i.e. are there distinctions between any lowercase and capital letters)
# OUTPUTS: two .csv files: minimalPairs.csv and minimalSets.csv,
#     saved in the same director as the input file


########### REQUIRED PACKAGES

# these lines are for installing packages if you don't already have them installed
# generally, you only need to run these once
#install.packages("tools")
#install.packages("rio")
#install.packages("stringdist")

library(tools) # functions used: file_ext
library(rio) # functions used: import
library(stringdist) # functions used: stringdistmatrix

########### USER INPUT SECTION

#filename = "" ## declare the filename (including path) of your input file

#sheetNumber = "Sheet1" ## specify the sheet name where the word list is located

#firstCellCol = 1 ## specify the column number of the wordlist
# e.g. for a list starting in cell A2, the column number is 1.


#firstCellRow = 2 ## specify the row number of the first item in the list
# e.g. for a list starting in cell A2, the row number is 2.

#digraphs = c("ch","kh") ## specify a list of diagraphs in the wordlist
#digraphs = NULL ## use this code to specify no digraphs 
# note: this code assumes the wordlist does not also have these combinations
# of characters as serarate and sequential graphs

#caseSensitivity = FALSE # is the wordlist case-sensitive? TRUE or FALSE
# (in other words, are there any distinctions between capital
# and lower cases versions of the same letter?)
# use a true value here if case denotes an orthographic distinction


########### functions for data validation of inputs via prompt

# this function prompts for an integer and ensures the respone is an integer
readInteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readInteger())
  }
  return(as.integer(n))
}

# this function prompts for a yes/no response and ensures the respone is "y" or "n"
readYesNo <- function()
{ 
  n <- readline(prompt="Enter y or n: ")
  if(!grepl("^[yn]$",n))
  {
    return(readYesNo())
  }
  return(as.character(n)=="y")
}


########### input prompts for undeclared inputs

if ( !exists("filename") & !exists("sheetNumber") & 
     !exists("firstCellCol") & !exists("firstCellRow") & 
     !exists("digraphs") & !exists("caseSensitivity")  ) {
message("Note: since you have not specified any inputs in the script,
you will now be prompted to specify them. In the future, you might find
it useful to specify some or all in the script (see 'USER INPUT SECTION').
You will only be prompted for the inputs you have not specified.

For example, this could be useful if you have many wordlists with a common
orthography. You could specify the digraphs and case sensitivity in the
script so that you are no longer prompted for them each time. By leaving
the filename unspecified you will get the file browser each time, so
you can specify different files across multiple runs easily.")

invisible(readline(prompt="Ready? press [enter] to continue"))
}

# if no filename is defined, bring up a file browser for the user to select one
if ( !exists("filename") ) {
  message("Choose the file with the wordlist...")
  filename <- file.choose()
}

workingDirectory = dirname(filename) # extract directory from filename
setwd(workingDirectory) # set director from filename
fileExtention = file_ext(filename) # extract file extension from filename

# if the relevant sheet name is not defined, prompt user to provide it
if ( !exists("sheetName") & fileExtention != "csv" ) {
  message("In which sheet of the file is the wordlist located?")
  sheetName = as.character(readline(prompt="Enter the name as a string with no quotes: "))
}

# if the column locating the wordlist is not defined, prompt user to provide it
if ( !exists("firstCellCol") ) {
  message("What is the column number of the wordlist?")
  firstCellCol = readInteger()
}

# if the row locating the beginning of the wordlist is not defined, prompt user to provide it
if ( !exists("firstCellRow") ) {
  message("What is the first row of the wordlist?
e.g. if there is one header row, this value would be 2")
  firstCellRow = readInteger()
}

# if a list of digraphs name is not defined, prompt user to provide it
if ( !exists("digraphs") ) {
  message("Are there any digraphs?")
  digraphsInput = as.character(readline(prompt="Enter a list separated by commas, no quotes. Leave blank for none: "))
}
digraphs = trimws(unlist(strsplit(digraphsInput, ",", fixed = TRUE))) #removes spaces, so that it doesn't matter if the user separates items with spaces in the prompt

# if the property of case sensitivity is not defined, prompt user to provide it
# this if/else statement first checks if a user erroneously defined a value of
# "y" or "n" in the script and converts it to an appropriate T/F value.
# this handling of incorrect inputs is a special case, due to the fact that
# the user is prompted for y/n if the value is undefined.
if ( exists("caseSensitivity") ) {
  if ( caseSensitivity == "y") {
    caseSensitivity = TRUE
  } else if ( caseSensitivity == "n") {
    caseSensitivity = FALSE
  }
} else {
  message("Is the wordlist case sensitive (distinctions between letters and
their capital counterparts)?")
  caseSensitivity = readYesNo()
}


########### start code execution timer

message("Beginning calculations now...

(Note that this can take some time.
A wordlist of >10,000 items may take an hour or more.

If you are short on time, use a .csv or .xlsx file as input -
.ods can take significantly longer.)")

# you can run everything left in the script simultaneously to ensure
# the timer is accurate (and if you don't need to see each step) 
start.time <- Sys.time()


########### read in the input file

# this if/else statement handles reading files of different extensions
# specifically, the sheet variable is not needed only for .csv files
# triggers an error if the file is not a csv, .ods, or .xlsx file.
if ( fileExtention == "csv") {
  file = import(filename)
} else if ( fileExtention == "ods" |fileExtention == "xlsx" ) {
  file = import(filename, which = sheetName)
} else {
  stop("this script only works with .csv, .ods, and .xlsx files. 
       You have selected some other file type.")
}

# extracts the wordlist from the sheet
wordlist = file[,firstCellCol]
wordlist = wordlist[!is.na(wordlist)]
wordlist = wordlist[wordlist != ""]

# removes leading rows from the wordlist
# the while loop is used so that the wordlist length need not be known
deletingRows = 2
while ( deletingRows < firstCellRow ) {
  wordlist = wordlist[-1]
  deletingRows = deletingRows + 1
}


########### process wordlist based on inputs

# if wordlist is not case sensitive, convert everything to lower case
if (caseSensitivity != TRUE) {
  wordlist = tolower(wordlist)
}

# this if statement creates a vector of graphs to replace the digraphs for the calculations
# this only executes if the length of digraphs is more than 0
# note that if there are no digraphs, the digraphs variable should have length = 0
if ( length(digraphs) > 0 ) {
  
  # this section of the if statement chooses which graphs to replace the digraphs
  # with a master set of unusual graphs is declared, and a subset of those
  # which are not found in the wordlist are used
  # note: if your orthography has many digraphs or many unusual graphs,
  # the code may run out of usable graphs and trigger an error. in this case,
  # you can append more values to the vector below, which aren't in your orthography
  # crucially: DO NOT add 0 to the master list. It is used in the outputs
  digraphReplacementsMasterList = c(1,2,3,4,5,6,7,8,9,"ƕ","ƙ","ơ","ƣ","ƨ","ư","ƶ","ƺ","ƽ","ǜ","ȭ")
  digraphReplacements=vector(length = 0)
  for (i in 1:length(digraphs)) {
    if ( sum(grepl(digraphReplacementsMasterList[i], wordlist, fixed = TRUE)) == 0 )
      digraphReplacements = c(digraphReplacements,digraphReplacementsMasterList[i])
  }
  
  # this section of the if statement replaces the digraphs
  for (i in 1:length(digraphs)) {
    wordlist = gsub(digraphs[i], digraphReplacements[i], wordlist)
  }
}

origWordlistSize = length(wordlist) # calculate size of original wordlist
wordlist = unique(wordlist) # remove duplicates from wordlist


########### calculate minimal pairs and sets

# this for loop is the most expensive part of the code
# it calculates the phonetic distance of each item in the wordlist
# to the entire wordlist, and then makes a list for each word
# of those words that are minimal paits.
# This is a relatively crude method, but it was designed to be
# robust to large wordlists and computers with little RAM.

minimalSetList = list()
for (i in 1:length(wordlist)) {
  distanceVector = as.vector(stringdistmatrix(wordlist,wordlist[i]))
  mpVector = distanceVector == 1
  iMinimalSet = c(wordlist[i],subset(wordlist, mpVector==TRUE))
  minimalSetList[[i]] <- iMinimalSet
}
minimalSetList = minimalSetList[lapply(minimalSetList,length)>1] # remove words with no minimal pairs

# this section of the code is for processing the results of
# the distance calculations into matrices suitable for output
# it processes two matrices, one for minimal pairs and one
# for minimal sets.
# The minimal sets calculated above are list elements of
# different lengths, so they need to be extracted and coerced
# to have the same length before binding them as a matrix.
# The minimal pairs are extracted from each minimal set.

nColMinimalSets = max(unlist(lapply(minimalSetList,length)))
minimalSets = matrix(NA,nrow = length(minimalSetList), ncol = nColMinimalSets)
minimalPairs = matrix(NA, nrow = 0, ncol = 2)
for (i in 1:length(minimalSetList)) {
  oneMinimalSetAsVector = unlist(minimalSetList[i])
  oneMinimalSetAsPairs = cbind(rep(oneMinimalSetAsVector[1],length(oneMinimalSetAsVector)-1),oneMinimalSetAsVector[-1])
  minimalPairs = rbind(minimalPairs,oneMinimalSetAsPairs)
  
  oneMinimalSetAsPaddedVector = c(oneMinimalSetAsVector, rep("",nColMinimalSets-length(minimalSetList[[i]])))
  minimalSets[i,] = oneMinimalSetAsPaddedVector
}
minimalPairs = unique(t(apply(minimalPairs,1,sort))) # remove duplicates from minimal pairs


# this section processes the minimal pairs further by adding two
# columns which contain the character that distinguishes the minimal pairs.
# 0 is used to represent the distinction for additions and deletions.
minimalPairs = cbind(minimalPairs,rep(NA,nrow(minimalPairs)),rep(NA,nrow(minimalPairs)))

word1difference = apply(minimalPairs, 1, function(x) setdiff(unlist(strsplit(x[1],split = "")),unlist(strsplit(x[2],split = ""))))
word1difference[lengths(word1difference) == 0] <- "0"
minimalPairs[,3] <- unlist(word1difference)

word2difference = apply(minimalPairs, 1, function(x) setdiff(unlist(strsplit(x[2],split = "")),unlist(strsplit(x[1],split = ""))))
word2difference[lengths(word2difference) == 0] <- "0"
minimalPairs[,4] <- unlist(word2difference)

colnames(minimalPairs) <- c("Word 1", "Word 2", "Distinction of 1", "Distinction of 2")


########### replace substituted graphs with the original digraphs

if ( length(digraphs) > 0 ) {
  for (i in 1:length(digraphs)) {
    minimalPairs = gsub(digraphReplacements[i], digraphs[i], minimalPairs)
    minimalSets = gsub(digraphReplacements[i], digraphs[i], minimalSets)
  }
}

########### write output files

write.table(minimalPairs, file = "minimalPairs.csv", sep=",", row.names = FALSE)

write.table(minimalSets, file = "minimalSets.csv", sep=",",  col.names=FALSE, row.names = FALSE)


########### stop code execution timer

end.time <- Sys.time()
time.taken = difftime(end.time,start.time, units = "secs")
time.taken.min = floor(time.taken/60)
time.taken.sec = round(time.taken - time.taken.min*60,1)


########### output summary message

message(c("Output report:
Your wordlist contains ",
          length(wordlist),
          " unique words (",
          round(length(wordlist)/origWordlistSize,2)*100,
          "% of ",
          origWordlistSize,
          " total words).
There are ",
          nrow(minimalPairs),
          " unique minimal pairs.
With this script, this wordlist, and your computer, the execution time was: ",
          time.taken.min,
          " min ",
          time.taken.sec,
          " sec.
Files saved to minimalPairs.csv and minimalSets.csv"))

