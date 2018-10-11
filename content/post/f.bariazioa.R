# Bariaziorik handiena duten unitateak identifikatu

# Bariazioa deskribatzeko funtzioak

# Bariazio handia erakusten duten guztiak
bariazioa.handia.batura <- function(deskribatzeko, taldeak){
    mtx <- psych::describeBy(deskribatzeko, taldeak,
                             mat = TRUE,
                             digits = 2)[, c(2,5:8,12,13)]
    # return( mtx)
    for(i in names(mtx[2:length(mtx)])){
        print(i)
        assign(paste("ord.", i, sep=""), bariazioa(mtx, mtx[,i]))
        
    }
    # alborapena/skew ere kontuan izanda
    ord.skew <- mtx[order(mtx$skew, decreasing = F), 1]
    emoitza.batura <- sort(# Ordenatu
        # as.numeric(
            union(ord.mean, c(
                union(ord.median, c(
                    union(ord.range, c(
                        union(ord.sd, 
                            union(ord.trimmed, ord.skew #)
            )))
        ))))
        ))
    return(emoitza.batura)
}

# Bariazio handia era guztietara erakusten dutenak
# Mean, trimead, median, range, sd.

bariazioa.handia.batera <- function(deskribatzeko, taldeak){
    mtx <- psych::describeBy(deskribatzeko, taldeak,
                             mat = TRUE,
                             digits = 2)[, c(2,5:8,12,13)]
    # return( mtx)
    for(i in names(mtx[2:length(mtx)])){
        #print(i)
        assign(paste("ord.", i, sep=""), bariazioa(mtx, mtx[,i]))
        
    }
    # alborapena/skew ere kontuan izanda
    ord.skew <- mtx[order(mtx$skew, decreasing = T), 1]
    emoitza.batera <- sort( 
        #as.numeric(
            intersect(ord.mean, c(
                intersect(ord.median, c(
                    intersect(ord.range, c(
                        intersect(ord.sd, 
                                  intersect(ord.trimmed, ord.skew#)
            )))
        ))))
        ))
    #emoitza.batera
    return(emoitza.batera)
}



# Lehenengo elementua da data framea
# Bigarren elementua da aldagaia (bektorea) 'df$ald' forman
bariazioa <- function(df, ald){
    df.helburua <- df[order(ald, decreasing = T),]
    
    laurdena <- 1:(round(nrow(df),0)/4)
    gehien <- df.helburua[laurdena,1]
    return(gehien)
}






