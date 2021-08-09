library(ggplot2)
library(ggforce)
if(!exists("geom_rrect", mode="function")) source("geom-rrect.R")

# library(extrafont)
# font_import(paths="./fonts", prompt=F)
# loadfonts(device="win")

# Draw Set Diagram, up to 3 sets
drawSetDiagram <- function(set1Name, set1Elements, set2Name, set2Elements, set3Name, set3Elements) {
  d <- ggplot() +  theme_void() + coord_fixed()
  
  # 1 set:
  if (missing(set2Elements) & missing(set3Elements)) {
    d <- d +
      geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=20), fill="blue", color="NA", size=0, alpha=0.4,
                 radius=unit(12, "pt")) +
      geom_text(aes(x=29, y=21, label='Omega'), size=8, parse = TRUE)
    d <- drawSet(d, 15, 10, 6, "red", set1Name, set1Elements)
  }
  # 2 sets: 2 possibilities:
  #   0, 1 intersection
  else if (missing(set3Elements)) {
    d <- d +
      geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=20), fill="blue", color="NA", size=0, alpha=0.4,
                 radius=unit(12, "pt")) +
      geom_text(aes(x=29, y=21, label='Omega'), size=8, parse = TRUE)
    d <- drawSet2(d, 15, 10, 6, set1Name, set1Elements, set2Name, set2Elements)
  }
  # 3 sets: 4 possibilities:
  #   0, 1, 2, 3 intersections
  else {
    d <- d +
      geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=30), fill="blue", color="NA", size=0, alpha=0.4,
                 radius=unit(12, "pt")) +
      geom_text(aes(x=29, y=31, label='Omega'), size=8, parse = TRUE)
    d <- drawSet3(d, 15, 15, 6, set1Name, set1Elements, set2Name, set2Elements, set3Name, set3Elements)
  }
  
  d
}

# draw One circle
drawSet <- function(plot, x, y, r, fill, name, elements, elementXOffset=0, elementYOffset=0, namePosition=0) {
  # ref: x=15 y=10 r=6
  step <- r/6
  x0 <- x
  y0 <- y
  d <- plot + geom_circle(aes(x0 = x0, y0 = y0, r = r), fill="red", color="NA", alpha=0.4, size=0)
  x <- x+elementXOffset
  y <- y+elementYOffset
  if (length(elements) == 0) {
  } else if (length(elements) == 1) {
    d <- d + geom_text(aes(x=x-step, y=y+step, label=elements[1]), size=8) +
      geom_point(aes(x=x, y=y), size=4)
  } else if (length(elements) == 2) {
    d <- d + geom_text(aes(x=x-step*2, y=y+step*2, label=elements[1]), size=8) +
      geom_text(aes(x=x+step*2, y=y-step*2, label=elements[2]), size=8) +
      geom_point(aes(x=x-step, y=y+step), size=4) +
      geom_point(aes(x=x+step, y=y-step), size=4)
  } else if (length(elements) == 3) {
    d <- d + geom_text(aes(x=x-step, y=y+step*2, label=elements[1]), size=8) +
      geom_text(aes(x=x-step*2, y=y-step*2, label=elements[2]), size=8) +
      geom_text(aes(x=x+step*2, y=y-step*2, label=elements[3]), size=8) +
      geom_point(aes(x=x, y=y+step), size=4) +
      geom_point(aes(x=x-step, y=y-step), size=4) +
      geom_point(aes(x=x+step, y=y-step), size=4)
  } else if (length(elements) == 4) {
    d <- d + geom_text(aes(x=x-step*2, y=y+step*2, label=elements[1]), size=8) +
      geom_text(aes(x=x+step*2, y=y+step*2, label=elements[2]), size=8) +
      geom_text(aes(x=x-step*2, y=y-step*2, label=elements[3]), size=8) +
      geom_text(aes(x=x+step*2, y=y-step*2, label=elements[4]), size=8) +
      geom_point(aes(x=x-step, y=y+step), size=4) +
      geom_point(aes(x=x+step, y=y+step), size=4) +
      geom_point(aes(x=x-step, y=y-step), size=4) +
      geom_point(aes(x=x+step, y=y-step), size=4)
  } else {
    d <- d + geom_text(aes(x=x, y=y+step*3.5, label=elements[1]), size=8) +
      geom_text(aes(x=x-step*3, y=y+step, label=elements[2]), size=8) +
      geom_text(aes(x=x+step*3, y=y+step, label=elements[3]), size=8) +
      geom_text(aes(x=x-step*2, y=y-step*2.5, label=elements[4]), size=8) +
      geom_text(aes(x=x+step*2, y=y-step*2.5, label=elements[5]), size=8) +
      geom_point(aes(x=x, y=y+step*2), size=4) +
      geom_point(aes(x=x-step*2, y=y+step*0.5), size=4) +
      geom_point(aes(x=x+step*2, y=y+step*0.5), size=4) +
      geom_point(aes(x=x-step, y=y-step*1.5), size=4) +
      geom_point(aes(x=x+step, y=y-step*1.5), size=4)
  }
  
  if (namePosition == 1 | namePosition == "tr") {
    # top right
    d <- d + geom_text(aes(x=x+step*4, y=y+step*6, label=name), size=8)
  } else if (namePosition == 2 | namePosition == "bl") {
    # bot left
    d <- d + geom_text(aes(x=x-step*4, y=y-step*6, label=name), size=8)
  } else if (namePosition == 3 | namePosition == "br") {
    # bot right
    d <- d + geom_text(aes(x=x+step*4, y=y-step*6, label=name), size=8)
  } else {
    # top left : default
    d <- d + geom_text(aes(x=x-step*4, y=y+step*6, label=name), size=8)
  }
  
  d
}

# draw Two circles
drawSet2 <- function(plot, x, y, r, set1Name, set1Elements, set2Name, set2Elements) {
  step <- r/6
  d <- plot
  
  setIntersection <- intersect(set1Elements, set2Elements)
  set1Elements <- setdiff(set1Elements, setIntersection)
  set2Elements <- setdiff(set2Elements, setIntersection)
  
  if (length(setIntersection) == 0) {
    # No intersection
    d <- drawSet(d, x-r-step, y, r, "red", set1Name, set1Elements)
    d <- drawSet(d, x+r+step, y, r, "blue", set2Name, set2Elements, namePosition = 1)
  } else {
    d <- drawSet(d, x-r+step*2, y, r, "red", set1Name, set1Elements, -step)
    d <- drawSet(d, x+r-step*2, y, r, "blue", set2Name, set2Elements, step, namePosition = 1)
    if (length(setIntersection) == 1) {
      # 1 elements
      d <- d + geom_text(aes(x=x, y=y+step, label=setIntersection[1]), size=8) +
        geom_point(aes(x=x, y=y), size=4)
    } else {
      # 2+ elements (only shows up to 2)
      d <- d + geom_text(aes(x=x-step, y=y+step, label=setIntersection[1]), size=8) +
        geom_text(aes(x=x-step, y=y-step, label=setIntersection[2]), size=8) +
        geom_point(aes(x=x, y=y+step), size=4) +
        geom_point(aes(x=x, y=y-step), size=4)
    }
  }
  
  d
}

# draw 3 circles
drawSet3 <- function(plot, x, y, r, set1Name, set1Elements, set2Name, set2Elements, set3Name, set3Elements) {
  d <- plot
  step <- r/6
  
  setIntersection123 <- intersect(intersect(set1Elements, set2Elements), set3Elements)
  setIntersection12 <- setdiff(intersect(set1Elements, set2Elements), setIntersection123)
  setIntersection23 <- setdiff(intersect(set2Elements, set3Elements), setIntersection123)
  setIntersection13 <- setdiff(intersect(set1Elements, set3Elements), setIntersection123)
  
  # change set elements to only unique
  set1Elements <- setdiff(setdiff(setdiff(set1Elements, setIntersection12), setIntersection13), setIntersection123)
  set2Elements <- setdiff(setdiff(setdiff(set2Elements, setIntersection12), setIntersection23), setIntersection123)
  set3Elements <- setdiff(setdiff(setdiff(set3Elements, setIntersection13), setIntersection23), setIntersection123)
  
  if (length(setIntersection123) > 0 | (length(setIntersection12) > 0 & length(setIntersection13) > 0 & length(setIntersection23) > 0)) {
    # all intersecting
      # set1
      d <- drawSet(d, 10, 18, 7, "red", set1Name, set1Elements, elementXOffset = -2*step, elementYOffset = 2*step, namePosition = "tl")
      # set2
      d <- drawSet(d, 20, 18, 7, "green", set2Name, set2Elements, elementXOffset = 2*step, elementYOffset = 2*step, namePosition = "tr")
      # set3
      d <- drawSet(d, 15, 10, 7, "yellow", set3Name, set3Elements, elementXOffset = 0, elementYOffset = -2*step, namePosition = "br")
      # 23
      if (length(setIntersection23) > 0) {
        d <- d + geom_text(aes(x=1, y=15, label=setIntersection23[1]), size=6) +
          geom_point(aes(x=18, y=14), size=4)
      }
      # 12
      if (length(setIntersection12) > 0) {
        d <- d + geom_text(aes(x=14, y=20, label=setIntersection12[1]), size=6) +
          geom_point(aes(x=15, y=19), size=4)
      }
      # 13
      if (length(setIntersection13) > 0) {
        d <- d + geom_text(aes(x=11, y=15, label=setIntersection13[1]), size=6) +
          geom_point(aes(x=12, y=14), size=4)
      }
      # 123
      if (length(setIntersection123) > 0) {
        d <- d + geom_text(aes(x=14, y=16.5, label=setIntersection123[1]), size=6) +
          geom_point(aes(x=15, y=15.5), size=4)
      }
  } else if (length(setIntersection12) == 0 & length(setIntersection13) == 0 & length(setIntersection23) == 0) {
    # no intersecting
    d <- drawSet(d, 8, 20, r, "red", set1Name, set1Elements, namePosition = "tr")
    d <- drawSet(d, 22, 20, r, "green", set2Name, set2Elements, namePosition = "tr")
    d <- drawSet(d, 15, 8, r, "yellow", set3Name, set3Elements, namePosition = "br")
  } else {
    Intersect2 <- FALSE
    if (length(setIntersection12) == 0) {
      if (length(setIntersection13) == 0) {
        # 23
      } else {
        if (length(setIntersection23) == 0) {
          # 13
        } else {
          # 13 and 23
          Intersect2 <- TRUE
        }
      }
    } else if (length(setIntersection13) == 0) {
      if (length(setIntersection23) == 0) {
        # 12
      } else {
        # 12 and 23
        Intersect2 <- TRUE
      }
    } else {
      # 12 and 13
      Intersect2 <- TRUE
    }
  }
  
  d
}

drawSetDiagram('G', c('a', '2', 'c'))
drawSetDiagram('A', c('a', '2', 'c'), 'B', c('a', '2', 'b'))
drawSetDiagram('A', c('a', '2', 'c'), 'B', c('a', '2', 'b'), 'D', c('a', '3', 'f'))
