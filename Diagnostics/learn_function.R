learn_function <- function(hist) {
  # My thoughts on this is to in this learn function create a network by analyzing the historical cases. 
  # The probability of the binary cases should be calculated from the hist diagram corresponding to the searched 
  # distribution. The normal distributed attribute should be fitted to a normal distribution and the values for the 
  # mean and the standard deviation in these cases should be returned. 
  
  # Creating the VTB-spots matrix 
  VTB_spots_0 <- length(which(hist[,3] == 0))
  VTB_spots_1 <- length(which(hist[,3] == 1))
  VTB_output <- data.frame("VTB_0" = VTB_spots_0/10000, "VTB_1" = VTB_spots_1/10000) 
  
  # Creating the smokes list-output 
  smokes_0 <- length(which(hist[,5] == 0))
  smokes_1 <- length(which(hist[,5] == 1))
  smokes_output <- data.frame("smokes_0" = smokes_0/10000, "smokes_1" = smokes_1/10000)
  
  # Now for the TB (index 4). REMEMBER: For this function, the first value is the one that is "given". 
  # Investigate this function if something in the future is not working! 
  calculating_one_dependence <- function(hist, vtb_value, tb_value) {
  VTB_TB_first <- hist[hist[,3] == vtb_value,]
  VTB_TB <- VTB_TB_first[VTB_TB_first[,4] == tb_value,]
  return(length(VTB_TB[,1])/length(VTB_TB_first[,1]))
  }
  
  TB_0_given_0 <- calculating_one_dependence(hist,0,0)
  TB_1_given_0 <- calculating_one_dependence(hist,0,1)
  TB_0_given_1 <- calculating_one_dependence(hist,1,0)
  TB_1_given_1 <- calculating_one_dependence(hist,1,1)
  
  # When creating these data frames the first position in the "given" columns is the probability of the binary 
  # value being 0 given the specific column value, the second position given that it is one. 
  # So my_data_farme[1,1] is given one what is the probability of this taking the value 0!
  TB_output <- data.frame("given_0" = c(TB_0_given_0, TB_1_given_0), "given_1" = c(TB_0_given_1, TB_1_given_1))
  
  
  
}