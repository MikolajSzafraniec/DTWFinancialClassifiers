######################### Class ShapeDescriptorParams ###########################

#################################################################################
###        Class which contains params of trigonometric transformations       ###
###                              of time series                               ###
#################################################################################

setClass(
  # Name of the class
  "TrigonometricTransformParams",
  
  # Slots
  slots = c(DimToApplyTransform = "integer",
            TransformType = "character"),
  
  # Default values
  prototype = list(
    DimToApplyTransform = c(1L),
    TransformType = "cosinus"
  ),
  
  # Checking validity of class member
  validity = function(object){
    
    DTAT <- object@DimToApplyTransform
    
    # Check if there is at least one dimension chosen to apply the transform
    if(length(DTAT) < 1){
      return("Transform must be applied to at least one dimension!")
    }
    
    # Check if all dimensions are positive integers
    if(any(DTAT < 1)){
      return("Each dimension number must be positive integer!")
    }
    
    TT <- object@TransformType
    
    # Check if there is only one type of transformation entered
    if(length(TT) != 1){
      return("There must be only one type of transformation entered")
    }
    
    # Check if the transformation type is one of the proper ones
    ProperTransformTypes <- c("sinus", "cosinus", "hilbert")
    if(!(TT %in% ProperTransformTypes)){
      return("Transform type must be one of below: \"sinus\", \"cosinus\", \"hilbert\"")
    }
  }
)

#################################################################################
###              Class which contains params of shape descriptors             ###
#################################################################################

setClass(
  # Name of the class
  "ShapeDescriptorParams",
  
  # Slots
  slots = c(Type = "character",
            Descriptors = "character",
            Additional_params = "list"),
  
  # Default values
  prototype = list(
    Type = "simple",
    Descriptors = "RawSubsequence",
    Additional_params = list(Weights = NULL, PAAWindow = NULL, slopeWindow = NULL)
  ),
  
  # Walidacja poprawności
  validity = function(object){
    
    # Test czy typ deskryptora jest wektorem jednoelementowym
    typeLenght <- length(object@Type)
    
    if(typeLenght != 1){
      return("Type of shape descriptor must be one-element vector with value equal to either \"simple\" or \"compound\"")
    }
    
    # Test czy typ deksryptora ma jedną z dwóch odpowiednich wartości
    if(!(object@Type %in% c("simple", "compound"))){
      return("Type of shape descriptors must be one of: - \"simple\"\n- \"compound\"")
    }
    
    descriptorLenght <- length(object@Descriptors)
    
    # Test czy długość wektora deskryptorów jest zgodna z jego typem
    if((object@Type == "simple" && descriptorLenght > 1) ||
       (object@Type == "compound" && descriptorLenght == 1)){
      return("Length of descriptors vector not matched to them type")
    }
    
    # Test czy wartości deskryptorów kształtu są unikatowe
    descriptorsUniqueLength <- length(unique(object@Descriptors))
    if(descriptorLenght != descriptorsUniqueLength){
      return("Vector of descriptors should contain unique values")
    }
    
    # Dopuszczalne wartości deskryptorów
    AcceptableDescriptors <- c("RawSubsequence", "PAADescriptor", "derivativeDescriptor",
                               "slopeDescriptor")
    
    # Test czt wszystkie zadeklarowane typy deskryptorów mają odpowiednią wartość
    if(!all(object@Descriptors %in% AcceptableDescriptors)){
      return("All values in descriptors vector must be one of following: 
             \"RawSubsequence\", \"PAADescriptor\", \"derivativeDescriptor\" 
             lub \"slopeDescriptor\"")
    }
    
    # Testy czy zadeklarowano odpowiednie wartosci okien w przypadku deskryptorów
    # typu ,,Slope" i "PAA"
    if("PAADescriptor" %in% object@Descriptors && 
       !is.integer(object@Additional_params$PAAWindow)){
      return("You must define PAAWindow as positive, integer value")
    }
    
    if("PAADescriptor" %in% object@Descriptors && 
       object@Additional_params$PAAWindow < 0){
      return("You must define PAAWindow as positive, integer value")
    }
    
    if("slopeDescriptor" %in% object@Descriptors && 
       !is.integer(object@Additional_params$slopeWindow)){
      return("You must define slopeWindow as positive, integer value")
    }
    
    if("slopeDescriptor" %in% object@Descriptors && 
       object@Additional_params$slopeWindow < 0){
      return("You must define slopeWindow as positive, integer value")
    }
    
    # Test czy parametr wag ma odpowiednią wartość
    if(descriptorLenght > 1 && is.null(object@Additional_params$Weights)){
      return("When using compound descriptor you have to decalare weights for 
             each of the descriptor")
    }
    
    weightsLength <- length(object@Additional_params$Weights)
    
    if((descriptorLenght > 1) && (weightsLength != descriptorLenght)){
      return("Lengths of descriptors vector and weights vector must match")
    }
    
    return(TRUE)
  }
)


