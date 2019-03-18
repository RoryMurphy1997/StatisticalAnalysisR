draw.a.circle <- function( centre, radius, Npt=100, add = FALSE )
{
  #centre gives the centre of the circle and radius the radius
  #the Npt argument gives how refined the circle should look
  #add == FALSE means don't add to the current plotting device; otherwise overlay
  #on the current plotting device
  t <- seq(0,2*pi,length=Npt)
  coords <- t(rbind( centre[1]+sin(t)*radius, centre[2]+cos(t)*radius))
  if( !add ) plot(coords, type="l") else points( coords, type="l") 
}