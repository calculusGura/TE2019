setClass("ProductData",
         representation(compList = "data.frame",
                        dpList = "data.frame",
                        compDPMatrix = "matrix",
                        constList = "data.frame",
                        constDPMatrix ="matrix",
                        dpdpMatrix = "matrix",
                        taskList = "data.frame",
                        taskDPMatrix ="matrix",
                        taskTaskMatrix = "matrix",
                        compStatusWeight = "data.frame",
                        taskStatusWeight = "data.frame"));


setClass("PropagationPath",
         representation(id = "character",
                        qualityScore = "numeric",
                        costScore = "numeric",
                        deliveryScore = "numeric",
                        solutionArea = "numeric",
                        path = "list",
                        dps = "character",
                        constraints = "character",
                        constDPMatrix ="matrix"));