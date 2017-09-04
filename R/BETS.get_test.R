BETS.get_test = function(code,from = "",to="",data.frame = FALSE, frequency = NULL){
    n = length(code)
    aux = length(from)
    aux2 = length(to)
    frequency_fild = c("M","A","D","Q","w")
    if(n > 1){
        ts = list()
        nomes = c()
        for(i in 1:n){nomes[i] = paste0("ts_",code[i])}
        if(aux > 1 && aux2 > 1){
            
            for(i in 1:n){
                ts[[i]] = get_series(code[i],from=from[i],to[i],data.frame=FALSE,frequency=NULL)
            }
            
        }else if(aux == 1 && aux2 == 1){
            i=1
            for(i in 1:n){
                ts[[i]] = get_series(code=108,from="",to="",data.frame=FALSE,frequency=NULL)
            }
        }else if(aux >1 && aux2>1){
            for(i in 1:n){
                ts[[i]] = get_series(code[i],from=from[i],to=to[i],data.frame=FALSE,frequency=NULL)
            }
        }    
        
        names(ts)<- nomes
        return(ts)
    }else{
        if(data.frame && (frequency %in% frequency_fild)) {
            return(get_series(code,from,to,data.frame=TRUE,frequency = frequency))
        }else{
            return(get_series(code,from,to,data.frame=FALSE,frequency = NULL))
        }
    }
}