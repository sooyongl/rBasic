scale_x_discrete( labels = function( labels ) { 
  fixedLabels <- c() 
  for ( l in 1:length( labels ) ) { 
    fixedLabels <- c( fixedLabels, paste0( ifelse( l %% 2 == 0, '', '\n' ), 
                                           labels[l] ) ) 
  }
  return( fixedLabels ) 
} )
' ', '\n'),
labels[l]))
}
return(fixedLabels)
})

# or

scale_x_discrete(labels = abbreviate)