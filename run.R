## source("prepare_data.R")
## 143.212s
Model <- lapply(1:numberOfEssaySet,function(k)
                with(Set[[k]],train.main(corpus1,feature1,Y1,Yrange)))
## 49.471s
Result <- lapply(1:numberOfEssaySet,function(k)
                 with(Set[[k]],predict.main(Model[[k]],corpus2,feature2)))
