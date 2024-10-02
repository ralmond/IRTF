## IRT Models

IRToutcomes <- function (model) {
  model$outcomes()
}

IRTparams <- function (model) {
  model$params()
}

IRTprobs <- function (model,thetas,values=IRToutcomes(model)) {
  model$probs(thetas,values)
}

IRTllike <- function(model,thetas,values=IRToutcomes(model)) {
  model$llike(thetas,values)
}

logit <- function (p) {log(p/(1-p))}
ilogit <- function (x) {1/(1+exp(-x))}
lilogit <- function (x) {-log(1+exp(-x))}

###

IRT2PL <- R6Class(
  classname="IRT2PL",
  public=list(
    b=0,
    a=1,
    initialize=function(b=0,a=1,loga=0) {
      self$b <- b
      if (missing(a) & !missing(loga)) {
        self$a <- exp(loga)
      } else {
        self$a <- a
      }
    },
    outcomes=function() {c(0,1)},
    params=function() {list(b=self$b,loga=self$loga)},
    probs=function(thetas,values=self$outcomes) {
      p <- ilogit(self$a*(thetas-self$b))
      sapply(values,function (y) ifelse(y=0,1-p,p))
    },
    llike=function(thetas,values=self$outcomes) {
     log(self$probs,values)
    }
  ),
  active=list(
    loga=function(value) {
      if(missing(values)) return(log(self$a))
      self$a <- exp(value)
    }
  )
)
