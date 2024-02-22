#FUNCTION: 
#Adjusts terminal link duration to a specified value if it is a pure S-delta.
#Note: Assumes any difference in probability is discriminable.
Sdelt <- function(TL_dur, TL_rp, new_Dur = 1) {
  duration <- ifelse(TL_dur == 0 & TL_rp == 0, 0,
                  ifelse(TL_dur > 0 & TL_rp == 0, new_Dur,
                         TL_dur))
  return(duration)
}


#FUNCTION: Probability of terminal reinforcement for a single alternative
TRp <- function(TL_p1, TL_rp1, TL_rp2) {
  p <-  TL_p1 * TL_rp1 + (1 - TL_p1) * TL_rp2
  return(p)
}


#FUNCTION: Average TL duration for a single Alternative
#Note: Assumes TLs are signaled.
TLdur <- function(TL_p1, TL_dur1, TL_dur2) {
  duration <- TL_p1 * TL_dur1 + (1 - TL_p1) * TL_dur2
  return(duration)
}


#FUNCTION: Primary Reinforcement Rate for one alternative
r <- function(TRp, IL1_dur, IL2_dur, TLdur, Sched_IL1, Sched_IL2,
             use_VI = FALSE) {
  #TRp = overall probability of terminal reinforcement for a alternative
  #TLdur = average terminal link duration of an alternative
  
  # Note: For concurrent VI schedules in the initial links, 
  # the average IL duration is used in the calculation of Big T.
  ILdur <- ifelse(Sched_IL1 == "VI" & Sched_IL2 == "VI" & use_VI == TRUE, 
                 1/((1/IL1_dur) + (1/IL2_dur)),
                 IL1_dur
  )
  
  #Time spent in IL waiting for reinforcement
  ILtime = ILdur * (1 / TRp)
  #Time spent in TL waiting for reinforcement
  TLtime = TLdur * (1 / TRp)
  #Terminal Reinforcement Rate
  rate = 1 / (ILtime + TLtime)
  
  return(rate)
}


# #FUNCTION: Big T
# Note: this function is designed to account for unequal initial link durations
# (See the OSF document for more details)
BigT <- function(IL1_dur, IL2_dur,
                 TRp_1, TRp_2,
                 r_1_Com, r_2_Com) {

  f_1 <- ((1/IL1_dur) / (1/IL1_dur + (1/IL2_dur))) *
    (TRp_1 / (TRp_1 + TRp_2))
  
  f_2 <- (1 - (1/IL1_dur) / (1/IL1_dur + (1/IL2_dur))) *
    (1 - TRp_1 / (TRp_1 + TRp_2))

  #Programmed Relative Proportion of Food Deliveries
  P_1 <- f_1  / (f_1 + f_2)
  
  # Big T
  return(P_1 * (1/r_1_Com) + (1 - P_1) * (1/r_2_Com))
}


#FUNCTION: Establish if an Alternative is Signalled

# It is assumed that, if two terminal links are specified in the spreadsheet
# and they have different reinforcement probabilities or different durations
# they must be signalled TLs, otherwise they are unsignalled.
# Two distinct TL stimuli with the same duration and reinforcement 
# probability are assumed to be functionally equivalent
# (i.e., can be interpreted as a single unsignalled TL).
Sig <- function(dur1, dur2, rein_p1, rein_p2) {
  return(ifelse(dur1 == 0 & dur2 == 0, FALSE,
                ifelse((dur1 == dur2) & (rein_p1 == rein_p2), FALSE, TRUE)
                )
        )
}


#FUNCTION: Average Delay Reduction
#e.g, Spetch & Dunn (1987)
DR_avg <- function(BigT, probRein, TLdur) {
  DR <- BigT * probRein - TLdur
  return(DR)
}


#FUNCTION: Bonus Delay Reduction
DR_bonus <- function(BigT, pRein_Splus, TLdur_Splus, probRein, TLdur, 
                    Sig, noBonus) {
  DR <- ifelse(Sig == TRUE & noBonus == FALSE,
               BigT * pRein_Splus - TLdur_Splus - BigT * probRein + TLdur,
               0)
  return(DR)
}


#FUNCTION: Determine S+ TL duration
sPlusDur <- function(Sig, TL_dur1, TL_dur2, TL_rp1, TL_rp2){
  ifelse(Sig == TRUE,
         ifelse(TL_rp1/TL_dur1 > TL_rp2/TL_dur2,
                TL_dur1,
                TL_dur2),
         NA)
}


#FUNCTION: Determine S+ TL Reinforcement Probability
sPlusRP <- function(Sig, TL_dur1, TL_dur2, TL_rp1, TL_rp2){
  ifelse(Sig == TRUE,
         ifelse(TL_rp1/TL_dur1 > TL_rp2/TL_dur2,
                TL_rp1,
                TL_rp2),
         NA)
}


#FUNCTION: Beta
beta_calc = function(betaBool, Sig, Sched1, Sched2, ILdur1, ILdur2, sPlusDur) {
  ILdur <- ifelse(Sched1 == "VI" & Sched2 == "VI",
                  1/(1/ILdur1 + 1/ILdur2),
                  ILdur1)
  
  beta <- ifelse(betaBool == TRUE & Sig == TRUE,
                 log10(1 + sPlusDur/ILdur),
                 1)
  
  return(beta)
}


#FUNCTION: DR Conditional Statements (see Squires and Fantino Eq. 3)
DR_cond = function(delRedA, delRedB, CP) {
  newCP = ifelse(delRedA > 0 & delRedB < 0, 1,
                 ifelse(delRedA < 0 & delRedB > 0, 0,
                        CP))
  return(newCP)
}


#NOTES:
#Function assumes two terminal links on each alternative.



#SiGN MODEL FUNCTION
#-------------------------------------------------------------------------------

SiGN = function(
  #Initial Link Duration
  ILa_dur,
  ILb_dur,
  
  #Terminal Link Duration
  TLa1_dur,
  TLa2_dur,
  
  TLb1_dur,
  TLb2_dur,
  
  #Terminal Link Entry Probability
  TLa1_p,
  TLa2_p,
  
  TLb1_p,
  TLb2_p,
  
  #Terminal Link Reinforcement Probability
  TLa1_rp,
  TLa2_rp,
  
  TLb1_rp,
  TLb2_rp,
  
  #Initial Link Schedules
  Sched_IL_a,
  Sched_IL_b,
  
  #S-delta (extinction stimulus duration)
  Sdelt_dur = 1,
  
  #Use Beta
  useBeta = TRUE,
  
  #Remove Bonus
  noBonus = FALSE
) {
  
  
  #Adjust Terminal Link Duration to S_delt_dur
  TLa1_dur = Sdelt(TLa1_dur, TLa1_rp, Sdelt_dur)
  TLa2_dur = Sdelt(TLa2_dur, TLa2_rp, Sdelt_dur)
  TLb1_dur = Sdelt(TLb1_dur, TLb1_rp, Sdelt_dur)
  TLb2_dur = Sdelt(TLb2_dur, TLb2_rp, Sdelt_dur)
  
  #Average Terminal Reinforcement Probability
  TRp_a = TRp(TLa1_p, TLa1_rp, TLa2_rp)
  TRp_b = TRp(TLb1_p, TLb1_rp, TLb2_rp)

  #Average Terminal Link Duration
  TLdur_a = TLdur(TLa1_p, TLa1_dur, TLa2_dur)
  TLdur_b = TLdur(TLb1_p, TLb1_dur, TLb2_dur)
  
  #Individual Alternative Reinforcement Rates
  r_a = r(TRp_a, ILa_dur, ILb_dur, TLdur_a, Sched_IL_a, Sched_IL_b, use_VI = FALSE)
  r_b = r(TRp_b, ILb_dur, ILa_dur, TLdur_b, Sched_IL_b, Sched_IL_a, use_VI = FALSE)
  
  #Common IL Reinforcement Rate (for VI VI schedules where switching occurs)
  #When VI VI schedules are not used r_a == r_a_Com and r_b == r_b_Com
  r_a_Com = r(TRp_a, ILa_dur, ILb_dur, TLdur_a, Sched_IL_a, Sched_IL_b, use_VI = TRUE)
  r_b_Com = r(TRp_b, ILb_dur, ILa_dur, TLdur_b, Sched_IL_b, Sched_IL_a, use_VI = TRUE)
  
  #Big T
  BigT = BigT(ILa_dur, ILb_dur, TRp_a, TRp_b, r_a_Com, r_b_Com)
  
  #Establish if each Alternative is Signalled
  A_Sig = Sig(TLa1_dur, TLa2_dur, TLa1_rp, TLa2_rp)
  B_Sig = Sig(TLb1_dur, TLb2_dur, TLb1_rp, TLb2_rp)
  
  #Establish S+ Duration
  A_Sig_dur <- sPlusDur(A_Sig, TLa1_dur, TLa2_dur, TLa1_rp, TLa2_rp)
  B_Sig_dur <- sPlusDur(B_Sig, TLb1_dur, TLb2_dur, TLb1_rp, TLb2_rp)
  
  #Establish S+ Reinforcement Prob
  A_Sig_rp <- sPlusRP(A_Sig, TLa1_dur, TLa2_dur, TLa1_rp, TLa2_rp)
  B_Sig_rp <- sPlusRP(B_Sig, TLb1_dur, TLb2_dur, TLb1_rp, TLb2_rp)
  
  #Delay Reduction Portions
  DRa_avg <- DR_avg(BigT, TRp_a, TLdur_a)
  DRa_bonus <- DR_bonus(BigT, A_Sig_rp, A_Sig_dur, TRp_a, TLdur_a, 
                        A_Sig, noBonus)
  
  DRb_avg <- DR_avg(BigT, TRp_b, TLdur_b)
  DRb_bonus <- DR_bonus(BigT, B_Sig_rp, B_Sig_dur, TRp_b, TLdur_b, 
                        B_Sig, noBonus)
  
  #Calculate Beta
  beta_a <- beta_calc(useBeta, A_Sig, Sched_IL_a, Sched_IL_b, 
                      ILa_dur, ILb_dur, A_Sig_dur)
  beta_b <- beta_calc(useBeta, B_Sig, Sched_IL_b, Sched_IL_a, 
                      ILb_dur, ILa_dur, B_Sig_dur)
  
  #Conditional Reinforcement
  CRa <- DRa_avg + DRa_bonus * beta_a
  CRb <- DRb_avg + DRb_bonus * beta_b

  # Full Equation
  SiGN_cp = (r_a * CRa) / ((r_a * CRa) + (r_b * CRb))
  
  # Conditional Statements
  SiGN_cp = DR_cond(CRa, CRb, SiGN_cp)
  
  #IL Schedule Warning
  FRtrip <- (Sched_IL_a == "FR" & Sched_IL_b == "FR" &
               ILa_dur == 1 & ILb_dur == 1)
    
  VItrip <- (Sched_IL_a == "VI" & Sched_IL_b == "VI")
    
  strMsg <- paste("The model is only equipped to test concurrent ",
                  "VIx VIx or FR1 FR1 initial link schedules.", sep = "")

  ifelse(FRtrip == TRUE | VItrip == TRUE,
      NA,
      warning(strMsg, call. = FALSE))
  
  #Bonus DR Warning
  ifelse(noBonus == TRUE,
        warning("Bonus delay-reduction is not being applied.", call. = FALSE),
        NA)
  
  
  #return(SiGN_cp)
  return(data.frame(SiGN_cp = SiGN_cp,
                    BigT = BigT,
                    CRa = CRa,
                    CRb = CRb,
                    DRa_avg = DRa_avg,
                    DRb_avg = DRb_avg,
                    DRa_bonus = DRa_bonus,
                    DRb_bonus = DRb_bonus,
                    r_a = r_a,
                    r_b = r_b,
                    r_a_Com = r_a_Com,
                    r_b_Com = r_b_Com,
                    beta_a = beta_a,
                    beta_b = beta_b,
                    A_Sig = A_Sig,
                    B_Sig = B_Sig,
                    A_Sig_dur = A_Sig_dur,
                    B_Sig_dur = B_Sig_dur,
                    A_Sig_rp = A_Sig_rp,
                    B_Sig_rp = B_Sig_rp,
                    TRp_a = TRp_a,
                    TRp_b = TRp_b,
                    TLdur_a = TLdur_a,
                    TLdur_b = TLdur_b,
                    TLa1_dur = TLa1_dur,
                    TLa2_dur = TLa2_dur,
                    TLb1_dur = TLb1_dur,
                    TLb2_dur = TLb2_dur))
} #End







