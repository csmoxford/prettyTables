\dontrun{
# Use some of the iris data set to create a table
iris2 = iris[c(1:10,51:60,101:110),]



# Create a new ConstructFlexTable object
bft = ConstructFlexTable(data = iris2[,c(5,1:4)])

# This is a reference class object so the methods which are called update the reference directly. As a consequence we don't need to set the values back to the object.

# Use the data in species column (1) to span rows with identical consecutive values
bft$SetSpanRowByColumn(j = 1)

# Use the data in species column (1) to set alternating colours
bft$SetColorRowAlternatingByColumn(colors = c("#FFFFFF","#CCCCCC"), j = 1)

# Use the data in species column (1) to set thicker horizontal border widths
bft$SetBorderWidthHorizontalByMatchingOnColumn(width = 2, 1)

# Set thicker vertical border widths
bft$SetBorderWidthVerticalByColumn(width = 2,index = c(0,1,3,5))

# Centre all text in the table
bft$SetTextAlign(align = "center")

bft$GetTable()

# we can remove some rows.
bft$CutRows(c(6:10))

bft$GetTable()

# And will then need to update the row span.
bft$SetSpanRowByColumn(j = 1)
bft$GetTable()

# Lets create some header rows
headft = ConstructFlexTable(data = matrix(c(
  "Species","Sepal","Sepal","Petal","Petal",
  "","Length","Width","Length","Width"
),byrow = TRUE, ncol = 5))

# Use the data in row 1 to span columns with identical consecutive values
headft$SetSpanColumnByRow(j = 1)

# span a number of rows using the range.
headft$SetSpanRowByRange(j = 1, from = 1, to = 2)

# Set horizontal borders
headft$SetBorderWidthHorizontalByRow(width= 2, index = c(0,2))

# Use the data in row 1 to set thicker vertical border widths
headft$SetBorderWidthVerticalByMatchingOnRow(width = 2,i = 1)

# Set a background color
headft$SetColorAll("#CCCCCC")

# Centre all text in the table
headft$SetTextAlign(align = "center")

# Make all text bold
headft$SetFontStyleByRow(style = "bold",index = c(1,2))

# with isHeaderRow defaulting to FALSE the row span for species works
# We can pass the body as a variable to GetTable
ft = headft$GetTable(bft)
ft
# Note we can pass as many ConstructFlexTable objects as we like to GetTable
# This is an alternative to
ft = GetFlexTable(headft, bft)

# we can then use addFlexTable (ReporteRs) to add to a .doxc or .pptx document.

# Note that we have not set header rows formally, as a consequence they will not repeat on page break in word.
# We can set them to be treated as header rows:
headft$SetHeaderRowByIndex(index = c(1,2))

# We do however lose any (vertical) row spans now because each row is processed using addHeaderRow (ReporteRs)
ft = headft$GetTable(bft)
ft

# It might have been quicker here to use the InsertHeaderRow function on the original object prior to formatting.
}
