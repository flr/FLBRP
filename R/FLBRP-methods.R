setMethod("recycle6d<-", signature(object="FLQuant", value="FLQuant"),
          function(object, value) {
            
            if (any(dim(value)[-6]>dim(object)[-6]))
              stop("dims in 2nd arg can't be greater than those in 1st")
            if (dims(value)$iter>1 & dims(object)$iter==1)
              object=propagate(object,dims(value)$iter)
            
            ## dims to expand in value
            nDim<-(1:6)[dim(value)!=pmax(dim(object),dim(value))]
            
            if (!all(dim(value)[nDim]==1 | dim(value)[nDim]==dim(object)[nDim]))
              stop("dims in 2nd arg can't be greater than 1 and != those in arg 1")
            
            return(sweep(FLQuant(0,dimnames=dimnames(object)), (1:6)[!(1:6 %in% nDim)], value, "+"))})     
