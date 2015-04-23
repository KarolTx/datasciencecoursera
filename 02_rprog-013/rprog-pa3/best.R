best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')

  if (!(state %in% data$State)) {
    stop('invalid state')
  }

  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }

  outcome_split <- unlist(strsplit(outcome, ' '))

  if (length(outcome_split) == 1) {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2))
  } else {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2), toupper(substring(outcome_split[2], 1, 1)), substring(outcome_split[2], 2))
  }

#  if (outcome == 'heart attack') {
#    col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
#  } else if (outcome == 'heart failure') {
#    col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
#  } else if (outcome == 'pneumonia') {
#    col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
#  }

  data_state <- data[data$State == state,]
  suppressWarnings(data_state[, c(col)] <- as.numeric(data_state[, c(col)]))
  data_order <- order(data_state[[col]], data_state$Hospital.Name)

  head(data_state[data_order,]$Hospital.Name, 1)
  
#  outcome[with(outcome, order(col, Hospital.Name)),]
#  m <- min(data_state[[col]])
#  print(sort(data_state[data_state[[col]] == m,]$Hospital.Name))
#  head(sort(data_state[data_state[[col]] == m,]$Hospital.Name), 1)
}
