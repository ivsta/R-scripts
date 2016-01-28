predict.Cartogram =
  #
  #  Essentials taken from Mark Newman's interp.c code.
  #
  #   This is a simple, inefficient version at present. 
  #
  function(object, x, y = NULL, ...)
  {
    if(missing(y)) {
      y = x[, 2]
      x = x[, 1]
    }
    
    ix = as.integer(x)
    iy = as.integer(y)
    dx = x - ix
    dy = y - iy
    
    # This could be done much more efficiently with some vectorized operations. 
    # and if we really care, it can be done very easily in C.
    
    sapply(object,  
           function(m) {
             sapply(seq(along = ix),
                    function(i)
                      pred(m, ix, iy, dx, dy, i))
           })
  }

pred = 
  function(m, ix, iy, dx, dy, i = 1) {
    # + need to go at the end of the line or the R parser
    # thinks these are separate expressions with the last 3 preceded by a +
    #  e.g.  1
    #        +2
    # which gives expression{ 1, 2}
    (1-dx[i])*(1-dy[i]) * m[, ix[i]][ iy[i] ] +
      dx[i] * (1-dy[i]) * m[, ix[i] + 1L][iy[i]]  + 
      (1-dx[i]) * dy[i]* m[, ix[i]][iy[i] + 1L]  + 
      dx[i] * dy[i]* m[, ix[i] + 1L][iy[i] + 1L]
  }


