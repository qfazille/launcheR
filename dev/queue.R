setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))

setClass(
    Class = "queue",
    slots = c(
        name = "character",
        group = "CharacterOrNULL",
        folder = "CharacterOrNULL",
        #executable = "", ###### Don't remember what this is for...
        batchs = "ListOrNULL"
    ),
    prototype = prototype(
        group = NULL,
        folder = NULL,
        #executable =
        batchs = NULL
    )
)

setMethod(f = "initialize", 
    signature = "queue", 
    definition = function(object, name = NULL, group = NULL, folder = NULL, batchs = NULL) {
        
        # Check that queue name not already running
        # Check there is no / or \ in queue name (because used in path)
        object@name      <- name
        object@group     <- group
        object@folder    <- folder
        object@batchs    <- batchs
    
        validObject(object)
        return(object)
    }
)

