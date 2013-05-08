

#setMethod('sp', signature(stock="FLBRP"),
sp.=function(stock){

    n2 =stock.n(stock)%*%exp(-m(stock))
    w2 =setPlusGroup(stock.wt(stock)[-1],41)
    res=apply(n2%*%w2,2:6,sum)

    return(res)}
#          )
