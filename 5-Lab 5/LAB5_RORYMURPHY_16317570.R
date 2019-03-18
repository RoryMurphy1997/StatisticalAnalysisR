#Declare working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/5-Lab 5")

#Part 1
#Draw a circle function
draw.a.circle <- function( centre, radius, Npt=100, add = FALSE, colour = "black",  line_type = "l", line_width = 1)
{
  #centre gives the centre of the circle and radius the radius
  #the Npt argument gives how refined the circle should look
  #add == FALSE means don't add to the current plotting device; otherwise overlay
  #on the current plotting device
  #colour determines the colour of the plot
  #line_type determines the type of line used for the circle
  #line_width determines the width of the line of the circle
  t <- seq(0,2*pi,length=Npt)
  coords <- t(rbind( centre[1]+sin(t)*radius, centre[2]+cos(t)*radius))
  if( !add ) plot(coords, type=line_type, col = colour, lwd = line_width ) else points( coords, type=line_type, col = colour, lwd = line_width) 
}
#Make plot square
par(pty="s")
#New colour test
draw.a.circle(c(5,5),2, colour = "blue")
#line type test
draw.a.circle(c(5,5),2, line_type = "b")
#line width test
draw.a.circle(c(5,5),2, line_width = 2)

#Part 2
#Make plot square
par(pty="s")
#Set up grid
plot(1, type="n", xlab="", ylab="", xlim=c(0, 21), ylim=c(0, 21), bty="n", xaxt='n', yaxt='n')
#Draw the nine circles
draw.a.circle(c(3,17),3,colour = 1, add = TRUE)
draw.a.circle(c(10,17),3,colour = 2, add = TRUE)
draw.a.circle(c(17,17),3,colour = 3, add = TRUE)
draw.a.circle(c(3,10),3,colour = 4, add = TRUE)
draw.a.circle(c(10,10),3,colour = 5, add = TRUE)
draw.a.circle(c(17,10),3,colour = 6, add = TRUE)
draw.a.circle(c(3,3),3,colour = 7, add = TRUE)
draw.a.circle(c(10,3),3,colour = 8, add = TRUE)
draw.a.circle(c(17,3),3,colour = 9, add = TRUE)
#Add text to circles
text(3,17,labels = "1", cex = 2, col = 1)
text(10,17,labels = "2", cex = 2, col = 2)
text(17,17,labels = "3", cex = 2, col = 3)
text(3,10,labels = "4", cex = 2, col = 4)
text(10,10,labels = "5", cex = 2, col = 5)
text(17,10,labels = "6", cex = 2, col = 6)
text(3,3,labels = "7", cex = 2, col = 7)
text(10,3,labels = "8", cex = 2, col = 8)
text(17,3,labels = "9", cex = 2, col = 9)

#Part 3
#Add *, 0 and # keys to keypad
#Make background black
par(bg = 1)
#Set up grid
plot(1, type="n", xlab="", ylab="", xlim=c(0, 30), ylim=c(0, 30), bty="n", xaxt='n', yaxt='n')
#Draw the twelve circles in green with a line width of 2
draw.a.circle(c(3,24),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(10,24),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(17,24),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(3,17),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(10,17),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(17,17),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(3,10),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(10,10),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(17,10),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(3,3),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(10,3),3,colour = 3, add = TRUE, line_width = 2)
draw.a.circle(c(17,3),3,colour = 3, add = TRUE, line_width = 2)

#Add text to circles coloured in red
text(3,24,labels = "1", cex = 2, col = 2)
text(10,24,labels = "2", cex = 2, col = 2)
text(17,24,labels = "3", cex = 2, col = 2)
text(3,17,labels = "4", cex = 2, col = 2)
text(10,17,labels = "5", cex = 2, col = 2)
text(17,17,labels = "6", cex = 2, col = 2)
text(3,10,labels = "7", cex = 2, col = 2)
text(10,10,labels = "8", cex = 2, col = 2)
text(17,10,labels = "9", cex = 2, col = 2)
text(3,3,labels = "*", cex = 2, col = 2)
text(10,3,labels = "0", cex = 2, col = 2)
text(17,3,labels = "#", cex = 2, col = 2)

#Reset par to default
par(bg = "white")
#Put those draws into a for loop