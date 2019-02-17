# tysabry struct connections



require(stringr)
require(ggplot2)
require(reshape2)
require(gdata)


tys.data.path = "/data/nap/tysabri/mtx4"

# D=tys.read.all()
# M=tys.melt(D)
# ggplot(M, aes(rep, value, col=sub)) + geom_point( ) + facet_wrap( ~ region, scales = 'free')


## Use
# reg = c( 'ThPostPar', 'ThPriM', 'ThPreM', 'ThS' )
# tys.plot.regions(M, reg )
# ggsave('tys-mtx-regions.pdf', width = 24, height = 16, units = 'in')

tys.plot.regions = function(M, reg)
{
	lreg = paste( 'l', reg, sep='')
	rreg = paste( 'r', reg, sep='')
	reg = c( lreg, rreg )
	R = M[ M$region %in% reg, ]

	p = ggplot(R, aes(rep, value, fill=sub)) + geom_bar(stat='identity', position='dodge') + facet_grid( region ~ sub*side)

	return(p)
}



tys.plot.all.regions = function(M)
{
	p = ggplot(M, aes(rep, value, col=sub)) + geom_point( ) + facet_wrap( ~ region)
	return(p)
}


tys.sel.regions = function( M )
{
    R = levels(M$region)
    R = R[c(5:9, 11:13, 15, 29,76 ) ]
    
    i = which(M$region %in% R)
    M = M[i,]
    
    return(M)
}

tys.melt = function(DF)
{
    M = melt( DF,  variable.name = 'region' )
    M$file = NULL
    return(M)
}


tys.read.all = function()
{
    DF = data.frame()
    
    F = dir( tys.data.path, pattern='*csv', full.names = TRUE )
    
    for ( f in F )
    {
        df = tys.read.file( f )
        DF = rbind( DF, df )
    }

    colnames(DF)[5:ncol(DF)] = tys.reg.names()
    return(DF)
}


tys.reg.names = function()
{
    
    n=read.xls( '/data/nap/tysabri/mtx/regio.xlsx', 1, header=FALSE)
    R=paste(n$V3, n$V2, sep='')
    return(R)

        
}



tys.read.file = function( fname )
{
    q= read.csv( fname, header = FALSE)
    x = str_split_fixed( fname, '-|\\.', 5 )
    df = cbind( data.frame( file=fname, sub=x[2], rep=x[3], side=x[4]), data.frame(q) )
    return(df)
}