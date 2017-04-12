library( ReporteRs )

setwd('/Users/wilsonpok/Desktop')

# Creation of doc, a pptx object (default template)
mydoc <- pptx( )

# check my layout names:
slide.layouts(mydoc)

mydoc <- addSlide( mydoc, "Two Content" )
# add into mydoc first 10 lines of iris
mydoc <- addTitle( mydoc, "First 10 lines of iris" )
mydoc <- addFlexTable( mydoc, vanilla.table(iris[1:10,] ) )

# add text into mydoc (and an empty line just before). Paragraph 
# properties will be those of the shape of the used layout.
mydoc <- addParagraph( mydoc, value = c("", "Hello World!") )

mydoc <- addSlide( mydoc, "Title and Content" )
# add a plot into mydoc 
mydoc <- addPlot( mydoc, function() barplot( 1:8, col = 1:8 ) )

filename <- "base_example.pptx" # the document to produce
# write mydoc 
writeDoc( mydoc, filename )
