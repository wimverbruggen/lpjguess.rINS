#' @title Resolve groups
#' @description Resolves included groups, such that PFTs will contain all parameters defined in higher-level groups (e.g. common)
#' 
#' @param ins Structured list with unresolved groups (direct output of read_ins())
#' @import dplyr
#' @import stringr
#' @return Structured list with resolved groups
#' @export
resolve_groups <- function(ins) {
  newins <- ins
  for(p in names(ins$pft)){
    newpft <- ins$pft[[p]] # Copy
    #newpft.imports <- newpft$imports # Which groups does this PFT import?
    while(length(newpft$imports)>0){
      for(gr in newpft$imports){
        
        # Group parameters
        gr.add <- ins$group[[gr]]
        
        # Other imports by this group
        gr.add.imports <- list() 
        if(length(gr.add$imports)>0) gr.add.imports <- gr.add$imports # Save separately
        gr.add$imports <- NULL # And then remove it from the group
        
        # Combine
        newpft <- c(newpft,gr.add)
        if(length(gr.add.imports)>0) newpft$imports <- unique(c(gr.add.imports,newpft$imports)) # Merge group imports with current newpft imports (and avoid duplicates)
        newpft$imports <- newpft$imports[newpft$imports!=gr] # Remove the processed import from newpft
        
      }
    }
    if(length(newpft$imports)==0) newpft$imports <- NULL # Finally, remove imports object (if condition should always be TRUE at this point!)
    newins$pft[[p]] <- newpft
  }
  
  # Remove duplicates. This will assume that the "last added one" is the one we want, just like in the INS file!
  newins[!duplicated(names(newins), fromLast = TRUE)]
  
  return(newins)
}
  