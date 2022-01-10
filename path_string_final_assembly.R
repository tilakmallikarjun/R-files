a <- character(0)
path_string_final_assembly<- function(x)
{
  parent <- subset(final_assembly,COMPONENT==x)$NEXT.ASSEMBLY
  if(identical(a,parent)) return(x)
  paste(path_string_final_assembly(parent),x, sep="/")
}
