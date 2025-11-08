## ---- Class (given; do not change) ----
setClass("sparse_numeric",
         slots = c(value = "numeric", pos = "integer", length = "integer")
)

## ---- Validity (must exist) ----
setValidity("sparse_numeric", function(object) {
  errs <- character()
  if (length(object@length) != 1L || is.na(object@length) || object@length < 0L)
    errs <- c(errs, "`length` must be a single nonnegative integer")
  if (length(object@value) != length(object@pos))
    errs <- c(errs, "`value` and `pos` must have the same length")
  if (any(is.na(object@value))) errs <- c(errs, "`value` cannot contain NA")
  if (any(is.na(object@pos)))   errs <- c(errs, "`pos` cannot contain NA")
  if (length(object@pos)) {
    if (!is.integer(object@pos)) errs <- c(errs, "`pos` must be integer")
    if (any(object@pos < 1L | object@pos > object@length))
      errs <- c(errs, "`pos` must be within [1, length]")
    if (is.unsorted(object@pos, strictly = TRUE))
      errs <- c(errs, "`pos` must be strictly increasing")
  }
  if (any(object@value == 0)) errs <- c(errs, "`value` must not include zeros")
  if (length(errs)) errs else TRUE
})

## ---- Coercions ----
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value  = as.numeric(from[nz]),
      pos    = as.integer(nz),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})

##  Helpers 
.mk  <- function(v, p, n) new("sparse_numeric", value = v, pos = p, length = as.integer(n))
.chk <- function(x, y) if (x@length != y@length) stop("Lengths differ", call. = FALSE)

## Generics 
setGeneric("sparse_add",       function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub",       function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult",      function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

##  Methods 
# add/sub: union of positions

# merge-based addition
.sparse_add_merge <- function(x, y) {
  .chk(x, y)
  i <- j <- 1L
  nx <- length(x@pos); ny <- length(y@pos)
  vals <- numeric(0); poss <- integer(0)
  
  while (i <= nx || j <= ny) {
    if (i <= nx && (j > ny || x@pos[i] < y@pos[j])) {
      p <- x@pos[i]; v <- x@value[i]; i <- i + 1L
    } else if (j <= ny && (i > nx || y@pos[j] < x@pos[i])) {
      p <- y@pos[j]; v <- y@value[j]; j <- j + 1L
    } else { # equal positions
      p <- x@pos[i]
      v <- x@value[i] + y@value[j]
      i <- i + 1L; j <- j + 1L
    }
    if (v != 0) { poss <- c(poss, as.integer(p)); vals <- c(vals, v) }
  }
  .mk(vals, poss, x@length)
}

# merge-based subtraction (x - y)
.sparse_sub_merge <- function(x, y) {
  .chk(x, y)
  i <- j <- 1L
  nx <- length(x@pos); ny <- length(y@pos)
  vals <- numeric(0); poss <- integer(0)
  
  while (i <= nx || j <= ny) {
    if (i <= nx && (j > ny || x@pos[i] < y@pos[j])) {
      p <- x@pos[i]; v <- x@value[i]; i <- i + 1L
    } else if (j <= ny && (i > nx || y@pos[j] < x@pos[i])) {
      p <- y@pos[j]; v <- -y@value[j]; j <- j + 1L
    } else { # equal positions
      p <- x@pos[i]
      v <- x@value[i] - y@value[j]
      i <- i + 1L; j <- j + 1L
    }
    if (v != 0) { poss <- c(poss, as.integer(p)); vals <- c(vals, v) }
  }
  .mk(vals, poss, x@length)
}

setMethod("sparse_add", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) .sparse_add_merge(x, y))

setMethod("sparse_sub", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) .sparse_sub_merge(x, y))

# mult: intersection of positions
setMethod("sparse_mult", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) {
            .chk(x, y)
            if (!length(x@pos) || !length(y@pos)) return(.mk(numeric(), integer(), x@length))
            com <- intersect(x@pos, y@pos); if (!length(com)) return(.mk(numeric(), integer(), x@length))
            ix <- match(com, x@pos); iy <- match(com, y@pos)
            v  <- x@value[ix] * y@value[iy]; keep <- which(v != 0)
            .mk(v[keep], as.integer(com[keep]), x@length)
          })

# cross product: scalar
setMethod("sparse_crossprod", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) {
            .chk(x, y)
            if (!length(x@pos) || !length(y@pos)) return(0)
            com <- intersect(x@pos, y@pos); if (!length(com)) return(0)
            ix <- match(com, x@pos); iy <- match(com, y@pos)
            sum(x@value[ix] * y@value[iy])
          })

# map operators
setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

## show() 
setMethod("show", "sparse_numeric", function(object) {
  cat("sparse_numeric: length =", object@length,
      " nonzeros =", length(object@pos), "\n")
})

##  plot(x, y): 
setMethod("plot", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) {
            .chk(x, y)
            plot(NA, xlim=c(1, max(1L, x@length)),
                 ylim=range(c(0, x@value, y@value)),
                 xlab="Index", ylab="Value", main="Sparse overlap", ...)
            if (length(x@pos)) points(x@pos, x@value, pch=1)
            if (length(y@pos)) points(y@pos, y@value, pch=3)
            com <- intersect(x@pos, y@pos); if (length(com)) points(com, 0*com, pch=16)
            legend("topright", bty="n", legend=c("x","y","overlap"), pch=c(1,3,16))
          })

## extra method: sum(
setMethod("sum", signature(x="sparse_numeric"),
          function(x, ..., na.rm = FALSE) sum(x@value, ..., na.rm = na.rm))
