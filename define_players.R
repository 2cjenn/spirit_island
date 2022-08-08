# Define player objects --------------------------------------------------------


# definition of S4 class
setClass("player", 
         slots=list(name="character",
                    spirit="character",
                    board="character"
         )
)

# creating an object using new() by passing class name and slot values
player1 <- new("player", name="Thomas", spirit="LSS", board="A")
player1

# using setMethod to set a method
setMethod("show", "player",
          function(object) 
          {
            cat(object@name, "played spirit", 
                object@spirit, "on board", 
                object@board, ".\n")
          }
)
player1