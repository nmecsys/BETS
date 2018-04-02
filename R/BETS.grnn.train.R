#' @title Train a General Regression Neural Network
#' 
#' @description Creates a set of probabilistic neural networks as proposed by \href{http://www.inf.ufrgs.br/~engel/data/media/file/cmp121/GRNN.pdf}{Specht [1991]}. The user provides a set of regressors and the function chooses which subset is the best, based on an accuracy measure (by default, the MAPE) between fited and actual values. These networks have only one parameter, the \code{sigma}, which is the standard deviation of each activation function (gaussian) of the pattern layer. Sigma can also be automatically chosen. This function builds on \link[grnn]{grnn-package}.  
#' 
#' @param train.set A \code{ts list} (a list of \code{ts} objects). The first element must be the dependent variable. The other elements, the regressors.
#' @param sigma A \code{numeric} or a \code{numeric vector}. The sigma parameter, that is, the standard deviation of the activation functions (gaussians) of the pattern layer. Can be either a fixed value or a range (a vector containing the minimum and the maximum values). 
#' @param step A \code{numeric} value. If \code{sigma} is a range, the user must provide a step value to vary sigma. The function is going to select the best sigma based on MAPE.
#' @param select A \code{boolean}. Must be set to \code{FALSE} if the regressors should not be chosen. The default is \code{TRUE}.
#' @param names A \code{character vector}. Optional. The names of the regressors. If not provided, indexes will be used and reported.  
#' 
#' @return A \code{list} of result objects, each representing a network. These objects are ordered by MAPE (the 20 best MAPEs) and its fields are:
#' 
#' \itemize{
#' \item{\code{accuracy}: A \code{numeric} value. Accuracy measure between the fitted and the actual series values. By default, the MAPE. In future versions, it will be possible to change it.}
#' \item{\code{fitted}: The fitted values, that is, one step ahead predicitions calculated by the trained net.}
#' \item{\code{net}: An object returned by the \link[grnn]{grnn} function. Represents a trained net. }
#' \item{\code{sigma}: A \code{numeric}. The sigma that was chosen, either by the user or by the function itself (in case \code{select} was set to \code{TRUE})}
#' \item{\code{regressors}: A \code{character vector}. Regressors that were chosen, either by the user or by the fuction itself (in case \code{select} was set to \code{TRUE})}
#' \item{\code{sigma.accuracy}: A \code{data.frame}. Sigma versus accuracy value of the corresponding trained network. Those networks were trained using the best set of regressors.}
#' \item{\code{residuals}: A \code{numeric vector}. Fitted values subtracted from the actual values.}
#' }
#' 
#' BETS.grnn.train also returns a diagnostic of training rounds and a \code{sigma} versus \code{accuracy} plot. 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import grnn forecast


BETS.grnn.train = function(train.set, sigma, step = 0.1, select = TRUE, names = NA){
  
  if(length(train.set) < 2 || !check.series(train.set, "Series list: train.")){
    return(NULL)
  }
  
  if(!is.na(names) && length(train.set) != length(names)){
    msg("ERROR")
    return(NULL)
  }
  
  train.n_elem = length(train.set[[1]])
  train.n_series = length(train.set)
  series = train.set[[1]]
  
  if(is.vector(sigma)){
    sigma = seq(sigma[1],sigma[2],step)
  }
  
  train_mt = matrix(nrow = train.n_elem, ncol = train.n_series)

  for(i in 1:train.n_series){
    train_mt[,i] = train.set[[i]]
  }
  
  results.list = vector(mode = "list")
  id = 1
  
  if(select){
    
    for(i in 1:(train.n_series-1)){
      
      trial = combn(2:train.n_series,i) 
      
      for(j in 1:ncol(trial)){
        
        sub_train = matrix(nrow = train.n_elem, ncol = nrow(trial)+1)
        sub_train[,1] = train_mt[,1]
        
        for(k in 1:nrow(trial)){
          
          ind = trial[k,j]
          sub_train[,k+1] = train_mt[,ind]
        }
        
        result = vector(mode = "list")
        result$mape = 1.797693e+308
        vec.sigmas = vector(mode = "numeric")
        vec.mapes = vector(mode = "numeric")
        
        for(s in sigma){
          
          nn = smooth(learn(sub_train),s)
          
          fitted = vector(mode = "numeric")
          sub_train_fit = as.matrix(sub_train[,-1])
          
          for(r in 1:nrow(sub_train)){
            fitted[r] = guess(nn, t(as.matrix(sub_train_fit[r,])))
          }
          
          acc = accuracy(fitted,sub_train[,1])[5]
          
          vec.sigmas = c(vec.sigmas,s)
          vec.mapes = c(vec.mapes,acc)

          if(acc < result$mape){
            
            result$mape = acc
            result$fitted = fitted
            result$net = nn
            result$sigma = s
            
            regs = trial[,j]
            
            if(!is.na(names)){
              result$regressors = names[regs] 
            }
            else {
              result$regressors = regs 
            }
          }
        }
        
        result$sigma.mape = cbind(sigma = vec.sigmas, mape = vec.mapes)
        result$series = series
        result$residuals = result$series - result$fitted
        result$id = id
        
        results.list[[id]] = result
        id = id + 1
      }
    }
  }
  
  len = length(results.list)
  rankm = data.frame(matrix(nrow = len, ncol = 4))
  names(rankm) = c("id","mape","regs","sigma")
  
  for(i in 1:len){
    rankm[i,"id"] = i
    rankm[i,"mape"] = results.list[[i]]$mape
    rankm[i,"regs"] = paste(results.list[[i]]$regressors, collapse = ",")
    rankm[i,"sigma"] = results.list[[i]]$sigma
  }
  
  rankm = head(rankm[order(rankm[,2]),],20)
  
  print("General Regression Neural Network")
  print(rankm)
  
  results = vector(mode = "list")
  for(i in 1:20){
    results[[i]] = results.list[[rankm[i,"id"]]]
  }
  
  plot(results[[1]]$sigma.mape, col = "royalblue", type = "b")
  
  return(results)
}