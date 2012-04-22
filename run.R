## source("prepare_data.R")
## load('data/all.RData')

## source('main/lasso(Simple+NBM).R')

print(system.time(
                  Model <- lapply(1:numberOfEssaySet,function(k)
                                  with(Set[[k]],train.main(corpus1,feature1,Y1,Yrange)))))
print(system.time(
                  Result <- lapply(1:numberOfEssaySet,function(k)
                                   with(Set[[k]],predict.main(Model[[k]],corpus2,feature2)))))
#Models <- append(Models,list(Model))
Results <- append(Results,list(Result))
