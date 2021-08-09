library(ggplot2)
library(ggforce)
library(gganimate)
library(transformr)
if(!exists("geom_rrect", mode="function")) source("geom-rrect.R")

createSetAnimation <- function(set1Name, set1Elements, set2Name, set2Elements, commonElement) {
  df <- read.table(text = "grp set x y sn x1 y1 x2 y2
                          a A 8   10 1  -2  2 2 0
                          b A 11  10 1  -2  2 4 0
                          a B 22  10 -1 1.5 2 -2 0
                          b B 19  10 -1 2   2 -4 0", header = TRUE)
  
  d <- ggplot(data = df, aes(x0=x, y0=y)) +
    theme_void() + coord_fixed() +
    geom_circle(aes(r=6, fill=factor(set)), color="NA", alpha=0.5, size=0, show.legend = FALSE) +
    geom_rrect(aes(xmin=0, xmax=30, ymin=0, ymax=20), fill="yellow", color="NA", size=0, alpha=0.2,
               radius=unit(12, "pt")) +
    geom_text(aes(x=29, y=21, label='Omega'), size=8, parse = TRUE) +
    geom_text(aes(x=x-5*sn, y=y+5, label=set), size=8) +
    #point labels 123
    geom_text(aes(x=x+x1-0.5, y=y+y1+0.5, label=c(set1Elements[1], set1Elements[1], set2Elements[1], set2Elements[1])), size=8) +
    geom_text(aes(x=x+x1-0.5, y=y-y1+0.5, label=c(set1Elements[2], set1Elements[2], set2Elements[2], set2Elements[2])), size=8) +
    geom_text(aes(x=x+x2-0.5, y=y+y2+0.5, label=commonElement), size=8) +
    #points 123
    geom_point(aes(x=x+x1+0.5, y=y+y1-0.5), size=4) +
    geom_point(aes(x=x+x1+0.5, y=y-y1-0.5), size=4) +
    geom_point(aes(x=x+x2+0.5, y=y+y2-0.5), size=4) +
    transition_states(grp, transition_length = 1, state_length = 2, wrap = TRUE) +
    view_step(pause_length = 1, step_length = 1, nsteps = 30)
  
  animate(d)
}

createSetAnimation('A', list('1', '2'), 'B', list('4', '5'), '3')

df <- read.table(text = "node intensity Lon Lat grp
                          SRC .9  40  60 1
                          SRC .9  40  60 2
                          SRC .9  40  60 3
                            TGT .89 80   40 1
                            TGT  .8 40 30 2
                            TGT .99 30     20 3", header = TRUE)
ggm <- ggplot(data = df, aes(x = Lon, y = Lat, size= intensity, colour=node, group = grp)) +
  geom_point(alpha=.5)+
  transition_states(node)+
  labs(title = "test")+ 
  shadow_wake(wake_length = 0.5)
ggm
