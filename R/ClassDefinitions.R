######################## Klasa SubsequenceSeries ##########################

###########################################################################
###            Klasa zawierająca podsekwencje szeregu czasowego         ###
###                       o dowolnej liczbie wymiarów                   ###
###########################################################################
setClass(
  # Nazwy klasy
  "SubsequenceSeries",
    # Sloty
  slots = c(Time = "timeDate",
            Subsequences = "list"),
  
  validity = function(object){
    
    # Test czy klasa obiektu zawierającego daty jest prawidłowa
    if(class(object@Time) != "timeDate"){
      reurn("Nieprawidłowa klasa obiektu zawierającego daty")
    }
    
    # Test czy wszystkie wymiary szeregu czasowego są danymi numerycznymi
    tsClasses <- lapply(object@Subsequences, "mode")
    if(!all(tsClasses == "numeric")){
      return("Co najmniej jeden z wymiarów szeregu czasowego zawiera dane w formacie
             nienumerycznym")
    }
      
    return(TRUE)
  }
)

######################## Klasa ShapeDescriptorsSeries ##########################

#################################################################################
###            Klasa zawierająca shapeDescriptory szeregu czasowego.          ###
###       Jest bliźniaczo podobna do SubsequenceSeries, osobno zdefiniowana   ###
###                            żeby był jakiś porządek                        ###
#################################################################################
setClass(
  # Nazwy klasy
  "ShapeDescriptorsSeries",
  
  # Sloty
  slots = c(Time = "timeDate",
            shapeDescriptors = "list"),
  
  validity = function(object){
    
    # Test czy klasa obiektu zawierającego daty jest prawidłowa
    if(class(object@Time) != "timeDate"){
      reurn("Nieprawidłowa klasa obiektu zawierającego daty")
    }
    
    # Test czy wszystkie wymiary szeregu czasowego są danymi numerycznymi
    tsClasses <- lapply(object@shapeDescriptors, "mode")
    if(!all(tsClasses == "numeric")){
      return("Co najmniej jeden z wymiarów szeregu czasowego zawiera dane w formacie
             nienumerycznym")
    }
    
    return(TRUE)
    }
)

######################### Klasa ShapeDescriptorParams ###########################

#################################################################################
###           Klasa zawierająca parametry deskryptorów kształtu               ###
#################################################################################

setClass(
  # Nazwa klasy
  "ShapeDescriptorParams",
  
  # Sloty
  slots = c(Type = "character",
            Descriptors = "character",
            Additional_params = "list"),
  
  # Wartości domyślne
  prototype = list(
    Type = "simple",
    Descriptors = "RawSubsequence",
    Additional_params = list(Weights = 1, PAAWindow = 1, slopeWindow = 1)
  ),
  
  # Walidacja poprawności
  validity = function(object){
    
    # Test czy parametry dodatkowe nie mają wartości NULL (nie zawsze są one potrzebne,
    # ale w momencie kiedy nie są zdefiniowane funkcje napisane w C++ mogą wyrzucać dziwne błędy)
    if(is.null(object@Additional_params$Weights) | is.null(object@Additional_params$PAAWindow) |
       is.null(object@Additional_params$slopeWindow)){
      return("All slots of Additional_params list (Weights, PAAWindow, slopeWindow)
             must be defined")
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


