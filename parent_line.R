a <- character(0)
parent_line <- function(x)
{
  parent <- subset(data2,COMPONENT==x)$NEXT.ASSEMBLY
  if(identical(a,parent)) return(x)
  c(x,parent_line(parent))
}
