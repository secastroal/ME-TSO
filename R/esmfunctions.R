# Ideas for functions for esm pack

# Merge a time invariant variable with the esm data in long format
merge.esm <- function(vars, id, esmdata, crossdata) {
  if(!is.data.frame(esmdata)) {
    esmdata <- data.frame(esmdata)
  }
  if(!is.data.frame(crossdata)) {
    crossdata <- data.frame(crossdata)
  }
  
  esmvarnames <- names(esmdata)
  esmnvars    <- length(esmvarnames)
  
  varnames    <- names(crossdata)
  nvars       <- length(varnames)
  
  if (!(is.character(id) | is.numeric(id))) 
    stop("Argument 'id' must either be a character or a numeric vector.")
  if (is.character(id)) {
    esm.id.pos <- charmatch(id[1], esmvarnames)
    if (length(esm.id.pos) == 0L) {
      stop("Variable '", id[1], "' not found in the longitudinal data frame.",
           call. = FALSE)
      }
    cross.id.pos <- charmatch(id[2], varnames)
    if (length(cross.id.pos) == 0L) {
      stop("Variable '", id[2], "' not found in the cross-sectional data frame.",
           call. = FALSE)
      }
    } else {
    esm.id.pos <- round(id[1])
    if (min(esm.id.pos) < 1 | max(esm.id.pos) > esmnvars) { 
      stop("ID longitudinal variable position must be between 1 and ", 
           esmnvars, ".")
    }
    cross.id.pos <- round(id[2])
    if (min(cross.id.pos) < 1 | max(cross.id.pos) > nvars) { 
      stop("ID cross-sectional variable position must be between 1 and ", 
           nvars, ".")
      }
    }
  
  if (!(is.character(vars) | is.numeric(vars))) 
    stop("Argument 'vars' must either be a character or a numeric vector.")
  if (is.character(vars)) {
    cross.vars.pos <- lapply(vars, function(x) {
      pos <- charmatch(x, varnames)
      if (is.na(pos)) 
        stop("Variable '", x, "' not found in the data frame.", 
             call. = FALSE)
      if (pos == 0L) 
        stop("Multiple matches for variable '", x, 
             "' in the data frame.", call. = FALSE)
      return(pos)
    })
    cross.vars.pos <- unique(unlist(cross.vars.pos))
  } else {
    cross.vars.pos <- unique(round(vars))
    if (min(cross.vars.pos) < 1 | max(cross.vars.pos) > nvars) { 
      stop("Variables positions must be between 1 and ", 
           nvars, ".")
    }
  }
  esmid     <- esmdata[, esm.id.pos]
  crossvars <- crossdata[, c(cross.id.pos, cross.vars.pos)]
  
  observations <- nobsub(esmid)
  
  temp <- data.frame(matrix(NA, nrow = nsub(esmid), ncol = 1 + length(vars)))
  colnames(temp) <- c("id", varnames[cross.vars.pos])
  temp[, 1] <- unique(esmid)
  
  for (i in 1:(dim(temp)[1])) {
    subj <- temp[i, 1]
    scores <- crossvars[crossvars[, 1] == subj, 2:(length(vars) + 1)]
    if (length(unlist(scores)) == 0L) {
      temp[i, 2:(length(vars) + 1)] <- rep(NA, length(vars))
    } else {
      temp[i, 2:(length(vars) + 1)] <- unlist(scores)
    }
  }
  rm(i)
  
  observations.match <- observations[match(temp[, 1], row.names(observations)), ]
  
  new.vars <- temp[rep(1:(dim(temp)[1]), times = observations.match), 2:(length(vars) + 1)]
  
  out <- data.frame(esmdata, new.vars)
  
  if (length(vars) == 1) {
    names(out)[dim(out)[2]] <- varnames[cross.vars.pos]
  }
  
  return(out)
}

# Compute the number of observations per person
nobsub <- function(id, data) {
  if (missing(data)) 
    data <- NULL
  no.data <- is.null(data)
  if (no.data) {
    data <- sys.frame(sys.parent())
  }
  else {
    if (!is.data.frame(data)) 
      data <- data.frame(data)
  }
  mf <- match.call()
  mf.id <- mf[[match("id", names(mf))]]
  id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (any(is.na(id))) 
    stop("Argument 'id' should not contain any NAs.")
  
  out <- data.frame(tapply(id, id, length))
  names(out) <- "nobs"
  
  return(out)
}

#!# It is done in calc.nomiss #!# Compute compliance per person and mean compliance

# a function for grand mean centering (for time invariant variables, 
# need to take a single value from each subject!)

calc.mcent.ti <- function(x, id, data, expand = TRUE) {
  if (missing(data)) 
    data <- NULL
  no.data <- is.null(data)
  if (no.data) {
    data <- sys.frame(sys.parent())
  }
  else {
    if (!is.data.frame(data)) 
      data <- data.frame(data)
  }
  mf <- match.call()
  mf.x <- mf[[match("x", names(mf))]]
  mf.id <- mf[[match("id", names(mf))]]
  x <- eval(mf.x, data, enclos = sys.frame(sys.parent()))
  id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  if (is.null(x)) 
    stop("Argument 'x' must be specified.")
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (any(is.na(id))) 
    stop("Argument 'id' should not contain any NAs.")
  
  var <- get.timeinvar(x, id, na.rm = FALSE)
  
  varm <- mean(var, na.rm = TRUE)
  
  var <- var - varm
  
  if (expand) {
    id.order     <- unique(id)
    observations <- nobsub(id)
    
    observations.match <- observations[match(id.order, row.names(observations)), ]
    var.match          <- var[match(id.order, names(var))]
    
    res <- var.match[rep(1:length(var.match), times = observations.match)]
  }
  else {
    res <- var
  }
  return(res)
}

# a function to expand a dataset with a preplanned number of days and beeps 
# to a full dataset (if missing beeps were removed); detect time-invariant 
# variables automatically and fill them in? (or make this an option); also 
# be able to exclude variables from this?
  
# a function that finds beeps where two items that are really opposites (e.g., cheerful & down) 
# are too similar

# separate esm per individual

split.esm <- function(id, data, include = NULL, exclude = NULL) {
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (is.null(data)) 
    stop("Argument 'data' must be specified.")
  
  if (!is.data.frame(data)) {
    data <- data.frame(data)
  }
  
  varnames <- names(data)
  nvars    <- length(varnames)
  
  if (!is.null(include) & !is.null(exclude)) {
    exclude <- NULL
    if (is.numeric(include)) {
      include <- varnames[include]
    }
    warning("Both arguments 'include' and 'exclude' were specified. ",
            "The following variables were included in the individual datasets: ",
            paste0(include, c(rep(", ", length(include) - 1), "."), collapse = ""))
  }
  
  if (is.null(include) & is.null(exclude)) {
    vars <- 1:length(varnames)
  }
  
  if (!is.null(include)) {
    if (!(is.character(include) | is.numeric(include))) 
      stop("Argument 'include' must either be a character or a numeric vector.")
    if (is.character(include)) {
      vars.pos <- lapply(include, function(x) {
        pos <- charmatch(x, varnames)
        if (is.na(pos)) 
          stop("Variable '", x, "' not found in the data frame.", 
               call. = FALSE)
        if (pos == 0L) 
          stop("Multiple matches for variable '", x, 
               "' in the data frame.", call. = FALSE)
        return(pos)
      })
      vars <- unique(unlist(vars.pos))
    } else {
      vars.pos <- unique(round(include))
      if (min(vars.pos) < 1 | max(vars.pos) > nvars) { 
        stop("Variables positions must be between 1 and ", 
             nvars, ".")
      }
      vars <- vars.pos
    }
  }
  
  if (!is.null(exclude)) {
    if (!(is.character(exclude) | is.numeric(exclude))) 
      stop("Argument 'exclude' must either be a character or a numeric vector.")
    if (is.character(exclude)) {
      vars.pos <- lapply(exclude, function(x) {
        pos <- charmatch(x, varnames)
        if (is.na(pos)) 
          stop("Variable '", x, "' not found in the data frame.", 
               call. = FALSE)
        if (pos == 0L) 
          stop("Multiple matches for variable '", x, 
               "' in the data frame.", call. = FALSE)
        return(pos)
      })
      vars <- (1:length(varnames))[-unique(unlist(vars.pos))]
    } else {
      vars.pos <- unique(round(exclude))
      if (min(vars.pos) < 1 | max(vars.pos) > nvars) { 
        stop("Variables positions must be between 1 and ", 
             nvars, ".")
      }
      vars <- (1:length(varnames))[-vars.pos]
    }
  }
  
  temp <- list()
  ids <- sort(unique(id))
  
  for (i in 1:length(ids)) {
    temp[[i]] <- data[id == ids[i], vars]
  }
  rm(i)
  
  if (is.numeric(ids)) {
    ids <- paste0("p", ids)
  }
  
  names(temp) <- ids
  
  return(temp)
}

# Function to recover unique ids


