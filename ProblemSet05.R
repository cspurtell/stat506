##### Problem 1. #####

### a. ###
# Definition
setClass(
  Class = "waldCI",
  slots = list(
    lb = "numeric",
    ub = "numeric",
    level = "numeric"
  )
)

# Validity function
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

# Constructor
makeWaldCI <- function(level, mean = NULL, sterr = NULL, lb = NULL, ub = NULL) {
  if (!is.null(mean) || !is.null(sterr)) {
    if (!is.numeric(sterr) || length(sterr) != 1 || is.na(sterr) || sterr <= 0) {
      stop("sterr must be a single positive number.")
    }
    
    if (!is.numeric(mean) || length(mean) != 1 || is.na(mean)) {
      stop("mean must be a single numeric value.")
    }
    
    z <- qnorm((1 + level) / 2)
    half_width <- z * sterr
    lb <- mean - half_width
    ub <- mean + half_width
    
  } else {
    if (is.null(lb) || is.null(ub)) {
      stop("You must supply either (mean, sterr) or (lb, ub).")
    }
  }
  
  obj <- new("waldCI", lb = lb, ub = ub, level = level)
  validObject(obj)
  obj
}

# Show method
setMethod("show", "waldCI", function(object) {
  cat(sprintf(
    "Wald CI (%.1f%%): [%.4f, %.4f]\n",
    100 * object@level,
    object@lb,
    object@ub
  ))
})

# Accessors
setGeneric("lb", function(x) standardGeneric("lb"))
setMethod("lb", "waldCI", function(x) x@lb)

setGeneric("ub", function(x) standardGeneric("ub"))
setMethod("ub", "waldCI", function(x) x@ub)

setGeneric("level", function(x) standardGeneric("level"))
setMethod("level", "waldCI", function(x) x@level)

setMethod("mean", "waldCI", function(x, ...) {
  (x@lb + x@ub) / 2
})

setGeneric("sterr", function(x) standardGeneric("sterr"))
setMethod("sterr", "waldCI", function(x) {
  z <- qnorm((1 + x@level) / 2)
  (x@ub - x@lb) / (2 * z)
})

# Setters
setGeneric("lb<-", function(x, value) standardGeneric("lb<-"))
setReplaceMethod("lb", "waldCI", function(x, value) {
  x@lb <- value
  validObject(x)
  x
})

setGeneric("ub<-", function(x, value) standardGeneric("ub<-"))
setReplaceMethod("ub", "waldCI", function(x, value) {
  x@ub <- value
  validObject(x)
  x
})

setGeneric("level<-", function(x, value) standardGeneric("level<-"))
setReplaceMethod("level", "waldCI", function(x, value) {
  x@level <- value
  validObject(x)
  x
})

setGeneric("mean<-", function(x, value) standardGeneric("mean<-"))
setReplaceMethod("mean", "waldCI", function(x, value) {
  se <- sterr(x)
  z  <- qnorm((1 + x@level) / 2)
  half_width <- z * se
  x@lb <- value - half_width
  x@ub <- value + half_width
  validObject(x)
  x
})

setGeneric("sterr<-", function(x, value) standardGeneric("sterr<-"))
setReplaceMethod("sterr", "waldCI", function(x, value) {
  if (value <= 0) stop("sterr must be positive.")
  m  <- mean(x)
  z  <- qnorm((1 + x@level) / 2)
  half_width <- z * value
  x@lb <- m - half_width
  x@ub <- m + half_width
  validObject(x)
  x
})

# Contains method