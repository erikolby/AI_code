learn <- function(hist) {
  # My thoughts on this is to in this learn function create a network by analyzing the historical cases. 
  # The probability of the binary cases should be calculated from the hist diagram corresponding to the searched 
  # distribution. The normal distributed attribute should be fitted to a normal distribution and the values for the 
  # mean and the standard deviation in these cases should be returned. 
  
  # NOTE: which function with two or more statements can be used, just put one & between them. 
  
  # Pneumonia: 
  Pn_0 <- length(which(hist[,1] == 0))
  Pn_1 <- length(which(hist[,1] == 1))
  Pn_output <- data.frame("Pn_0" = (Pn_0+1)/10002, "Pn_1" = (Pn_1+1)/10002)
  
  # Creating the VTB-spots matrix 
  VTB_spots_0 <- length(which(hist[,3] == 0))
  VTB_spots_1 <- length(which(hist[,3] == 1))
  VTB_output <- data.frame("VTB_0" = (VTB_spots_0+1)/10002, "VTB_1" = (VTB_spots_1+1)/10002) 
  
  # Creating the smokes list-output 
  smokes_0 <- length(which(hist[,5] == 0))
  smokes_1 <- length(which(hist[,5] == 1))
  smokes_output <- data.frame("smokes_0" = (smokes_0+1)/10002, "smokes_1" = (smokes_1+1)/10002)
  
  # Now for the TB (index 4). REMEMBER: For this function, the first value is the one that is "given". 
  calculating_one_dependence <- function(hist, given_value, prob_value, given_index, prob_index) {
    VTB_TB_first <- hist[hist[,given_index] == given_value,]
    VTB_TB <- VTB_TB_first[VTB_TB_first[,prob_index] == prob_value,]
    return((length(VTB_TB[,1])+1)/(length(VTB_TB_first[,1])+2))
  }
  
  TB_0_given_0 <- calculating_one_dependence(hist,0,0,3,4)
  TB_1_given_0 <- calculating_one_dependence(hist,0,1,3,4)
  TB_0_given_1 <- calculating_one_dependence(hist,1,0,3,4)
  TB_1_given_1 <- calculating_one_dependence(hist,1,1,3,4)
  
  # When creating these data frames the first position in the "given" columns is the probability of the binary 
  # value being 0 given the specific column value, the second position given that it is one. 
  # So my_data_farme[1,1] is given one what is the probability of this taking the value 0!
  TB_output <- data.frame("given_VTB_0" = c(TB_0_given_0, TB_1_given_0), "given_VTB_1" = c(TB_0_given_1, TB_1_given_1))
  
  # Bronchitis depending on smoke: 
  BR_0_given_0 <- calculating_one_dependence(hist,0,0,5,7)
  BR_1_given_0 <- calculating_one_dependence(hist,0,1,5,7)
  BR_0_given_1 <- calculating_one_dependence(hist,1,0,5,7)
  BR_1_given_1 <- calculating_one_dependence(hist,1,1,5,7)
  
  BR_output <- data.frame("given_smokes_0" = c(BR_0_given_0, BR_1_given_0), "given_smokes_1" = c(BR_0_given_1, BR_1_given_1))
  
  # Lung cancer depending on smoke: 
  LC_0_given_0 <- calculating_one_dependence(hist,0,0,5,6)
  LC_1_given_0 <- calculating_one_dependence(hist,0,1,5,6)
  LC_0_given_1 <- calculating_one_dependence(hist,1,0,5,6)
  LC_1_given_1 <- calculating_one_dependence(hist,1,1,5,6)
  
  LC_output <- data.frame("given_smokes_0" = c(LC_0_given_0, LC_1_given_0), "given_smokes_1" = c(LC_0_given_1, LC_1_given_1))
  
  # Temperature depending on Pneumonia: (NOTICE: the mean is the first value, the sd is the second. These can be used to calculate the probability of expecting a specific value on the temp) 
  Pn_0_temp <- hist[hist[,1] == 0,]
  Pn_1_temp <- hist[hist[,1] == 1,]
  temp_output <- data.frame("given_Pn_0" = c(mean(Pn_0_temp[,2]), sd(Pn_0_temp[,2])), 
                            "given_Pn_1" = c(mean(Pn_1_temp[,2]), sd(Pn_1_temp[,2])))
  
  # Dyspnea (9) depending on lung-cancer (6) and bronchitis(7): 
  calculating_two_dependencies <- function(hist, first, second, investigated, index_one, index_two, index_three) {
    LC <- hist[hist[,index_one] == first,]
    LC_Br <- LC[LC[,index_two] == second,]
    Dy_LC_Br <- LC_Br[LC_Br[,index_three] == investigated,]
    return((length(Dy_LC_Br[,1])+1)/(length(LC_Br[,1])+2))
  }
  
  Dy_0_given_LC_0_Br_0 <- calculating_two_dependencies(hist, 0, 0, 0, 6, 7, 9)
  Dy_1_given_LC_0_Br_0 <- calculating_two_dependencies(hist, 0, 0, 1, 6, 7, 9)
  Dy_0_given_LC_0_Br_1 <- calculating_two_dependencies(hist, 0, 1, 0, 6, 7, 9)
  Dy_1_given_LC_0_Br_1 <- calculating_two_dependencies(hist, 0, 1, 1, 6, 7, 9)
  Dy_0_given_LC_1_Br_0 <- calculating_two_dependencies(hist, 1, 0, 0, 6, 7, 9)
  Dy_1_given_LC_1_Br_0 <- calculating_two_dependencies(hist, 1, 0, 1, 6, 7, 9)
  Dy_0_given_LC_1_Br_1 <- calculating_two_dependencies(hist, 1, 1, 0, 6, 7, 9)
  Dy_1_given_LC_1_Br_1 <- calculating_two_dependencies(hist, 1, 1, 1, 6, 7, 9)
  
  Dy_output <- data.frame("given_LC_0_Br_0" = c(Dy_0_given_LC_0_Br_0, Dy_1_given_LC_0_Br_0),
                          "given_LC_0_Br_1" = c(Dy_0_given_LC_0_Br_1, Dy_1_given_LC_0_Br_1),
                          "given_LC_1_Br_0" = c(Dy_0_given_LC_1_Br_0, Dy_1_given_LC_1_Br_0),
                          "given_LC_1_Br_1" = c(Dy_0_given_LC_1_Br_1, Dy_1_given_LC_1_Br_1))
  
  # X-ray result (8) depending on Pneumonia (1), Tuberculosis (4) and Lung Cancer (6): 
  calculating_three_dependencies <- function(hist, first, second, third, investigated, index_one, index_two, index_three, index_four) {
    Pn <- hist[hist[,index_one] == first,]
    Pn_TB <- Pn[Pn[,index_two] == second,]
    Pn_TB_LC <- Pn_TB[Pn_TB[,index_three] == third,]
    Xray_Pn_TB_LC <- Pn_TB_LC[Pn_TB_LC[,index_four] == investigated,]
    return((length(Xray_Pn_TB_LC[,1])+1)/(length(Pn_TB_LC[,1])+2))
  }
  
  Xray_0_given_Pn_0_TB_0_LC_0 <- calculating_three_dependencies(hist, 0, 0, 0, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_0_TB_0_LC_0 <- calculating_three_dependencies(hist, 0, 0, 0, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_0_TB_0_LC_1 <- calculating_three_dependencies(hist, 0, 0, 1, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_0_TB_0_LC_1 <- calculating_three_dependencies(hist, 0, 0, 1, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_0_TB_1_LC_0 <- calculating_three_dependencies(hist, 0, 1, 0, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_0_TB_1_LC_0 <- calculating_three_dependencies(hist, 0, 1, 0, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_1_TB_0_LC_0 <- calculating_three_dependencies(hist, 1, 0, 0, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_1_TB_0_LC_0 <- calculating_three_dependencies(hist, 1, 0, 0, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_0_TB_1_LC_1 <- calculating_three_dependencies(hist, 0, 1, 1, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_0_TB_1_LC_1 <- calculating_three_dependencies(hist, 0, 1, 1, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_1_TB_1_LC_0 <- calculating_three_dependencies(hist, 1, 1, 0, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_1_TB_1_LC_0 <- calculating_three_dependencies(hist, 1, 1, 0, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_1_TB_0_LC_1 <- calculating_three_dependencies(hist, 1, 0, 1, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_1_TB_0_LC_1 <- calculating_three_dependencies(hist, 1, 0, 1, 1, 1, 4, 6, 8)
  
  Xray_0_given_Pn_1_TB_1_LC_1 <- calculating_three_dependencies(hist, 1, 1, 1, 0, 1, 4, 6, 8)
  Xray_1_given_Pn_1_TB_1_LC_1 <- calculating_three_dependencies(hist, 1, 1, 1, 1, 1, 4, 6, 8)
  
  Xray_output <- data.frame("given_Pn_0_TB_0_LC_0" = c(Xray_0_given_Pn_0_TB_0_LC_0, Xray_1_given_Pn_0_TB_0_LC_0),
                           "given_Pn_0_TB_0_LC_1" = c(Xray_0_given_Pn_0_TB_0_LC_1, Xray_1_given_Pn_0_TB_0_LC_1),
                           "given_Pn_0_TB_1_LC_0" = c(Xray_0_given_Pn_0_TB_1_LC_0, Xray_1_given_Pn_0_TB_1_LC_0),
                           "given_Pn_1_TB_0_LC_0" = c(Xray_0_given_Pn_1_TB_0_LC_0, Xray_1_given_Pn_1_TB_0_LC_0),
                           "given_Pn_0_TB_1_LC_1" = c(Xray_0_given_Pn_0_TB_1_LC_1, Xray_1_given_Pn_0_TB_1_LC_1),
                           "given_Pn_1_TB_1_LC_0" = c(Xray_0_given_Pn_1_TB_1_LC_0, Xray_1_given_Pn_1_TB_1_LC_0),
                           "given_Pn_1_TB_0_LC_1" = c(Xray_0_given_Pn_1_TB_0_LC_1, Xray_1_given_Pn_1_TB_0_LC_1),
                           "given_Pn_1_TB_1_LC_1" = c(Xray_0_given_Pn_1_TB_1_LC_1, Xray_1_given_Pn_1_TB_1_LC_1))
  
  
  return(list("Pn" = Pn_output, "VTB" = VTB_output, "smokes" = smokes_output, "temperature" = temp_output,
              "TB" = TB_output, "BR" = BR_output, "LC" = LC_output, "Dy" = list("LC_0" = list("BR_0" = Dy_output[[1]], 
                                                                                              "BR_1" = Dy_output[[2]]),
                                                                                "LC_1" = list("BR_0" = Dy_output[[3]],
                                                                                              "BR_1" = Dy_output[[4]])), 
              "Xray" = list("Pn_0" = list("TB_0" = list("LC_0" = Xray_output[[1]], "LC_1" = Xray_output[[2]]),
                                          "TB_1" = list("LC_0" = Xray_output[[3]], "LC_1" = Xray_output[[5]])),
                            "Pn_1" = list("TB_0" = list("LC_0" = Xray_output[[4]], "LC_1" = Xray_output[[7]]),
                                          "TB_1" = list("LC_0" = Xray_output[[6]], "LC_1" = Xray_output[[8]])))))
                            
  
}