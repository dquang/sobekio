setClass('HisFile',
         representation = representation(
           path = 'character',
           hia = 'character',
           nPar = 'integer',
           nLoc = 'integer',
           title = 'character',
           locs = 'data.table')
)

hisF <- new('HisFile',
            path = 'c:/so21503/vgtb.lit/3/calcpnt.his')
setGeneric('getLocation',
          function(object,...) standardGeneric('getLocation')
            )
setMethod('getLocation',
          'HisFile',
          function(obj){
            obj@locs <- sobekio::his_location(obj@path)
            # return(rel)
          })
tmp <- getLocation(hisF)
class(hisF)
