# R scripts for set visualization

### drawSetDiagram.R
Creates a venn diagram of up to 3 sets, given lists of elements which are single alphanumeric characters.
Each region unique to a set can show up to 5 elements. Intersection regions can show up to 2 elements, except for the intersection of 3 sets which only shows 1.
Depends on `geom-rrect.R` to create rounded rectangles.

### setAnimation.R
Creates an animation showing two sets with a common element merging.

### setdiagrams_example.Rmd
An R Markdown document showcasing how to use the drawSetDiagram script to create visualizations. Also has an example of setAnimation.

## R-set-operations , R-venn-diagrams
Testing using other existing R packages to draw venn diagrams.
