rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }
  
  if (!((num %in% c('best', 'worst')) | is.numeric(num))) {
    stop('invalid num')
  }

  outcome_split <- unlist(strsplit(outcome, ' '))
  
  if (length(outcome_split) == 1) {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2))
  } else {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2), toupper(substring(outcome_split[2], 1, 1)), substring(outcome_split[2], 2))
  }

  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  states <- unique(data$State)

  # column restriction
  data_sub <- data[, c('Hospital.Name', 'State', col)]
  # converting to numbers
  suppressWarnings(data_sub[, c(col)] <- as.numeric(data_sub[, c(col)]))

  sort_select <- function(data_part_in) {
    # removing NAs
    data_part <- na.omit(data_part_in)
    # sorting
    data_part <- data_part[order(data_part[col], data_part['Hospital.Name']),]

    if (num == 'best') {
      return(head(data_part[, c('Hospital.Name', 'State')], 1))
    }
    
    if (num == 'worst') {
      return(tail(data_part[, c('Hospital.Name', 'State')], 1))
    }
    
    if (num > nrow(data_part)) {
      return(data.frame(Hospital.Name=NA, State=data_part[1, c('State')])) #, row.names=c(data_part[1]$State)))
    } else {
      return(data_part[num, c('Hospital.Name', 'State')])
    }
  }

  #res <- ddply(data_sub, .(State), sort_select)
  res <- by(data_sub, data_sub$State, FUN=sort_select)
  res <- data.frame(do.call("rbind", res))
  names(res) <- c('hospital', 'state')
  return(res)
}
