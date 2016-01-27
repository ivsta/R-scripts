filename = system.file("data", "uspop.dat", package = "Rcartogram")
pop = read.table(filename)
grid = cartogram(pop)

predict(grid, 3.1, 4.5)

# note that this is equivalent to using the interp application in
# the cart(ogram) distribution with inputs as 2.1 3.4
# i.e. echo "2.1 3.4" | interp 1024 512 output.dat
#        

# now predict lots of values.
o = system.time(predict(grid, 
                        runif(10000, 1, 1024), 
                        runif(10000, 1, 512)))