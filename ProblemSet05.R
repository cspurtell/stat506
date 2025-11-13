##### Problem 1. #####

### a. ###
setClass(
  Class = "waldCI",
  slots = list(
    lb = "numeric",
    ub = "numeric",
    level = "numeric"
  )
)

setValidity("waldCI", function(object) {
  msgs <- character()
  if (length(object@lb) != 1) {
    msgs <- c(msgs, "lb must be a numeric of length 1.")
    }
  if (length(object@ub) != 1) {
    msgs <- c(msgs, "ub must be a numeric of length 1.")
    }
  if (length(object@level) != 1) {
    msgs <- c(msgs, "level must be a numeric of length 1.")
    }
  if (is.na(object@lb) || is.na(object@ub) || is.na(object@level)) {
    msgs <- c(msgs, "lb, ub, and level must not be NA.")
    }
  if (!is.finite(object@lb) || !is.finite(object@ub)) {
    msgs <- c(msgs, "lb and ub must be finite.")
    }
  if (object@level <= 0 || object@level >= 1) {
    msgs <- c(msgs, "level must be strictly between 0 and 1.")
    }
  if (object@lb >= object@ub) {
    msgs <- c(msgs, "lb must be strictly less than ub.")
    }
  if (length(msgs) == 0) {
    TRUE 
    } else msgs
})

makeWaldCI <- function(level, mean = NULL, sterr = NULL, lb = NULL, ub = NULL) {
  using_mean <- !is.null(mean) || !is.null(sterr)
  
  
  
}