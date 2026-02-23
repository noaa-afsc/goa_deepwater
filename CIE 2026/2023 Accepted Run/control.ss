#V3.30.13-safe;_2019_03_09;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0
#Stock Synthesis (SS) is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
#_data_and_control_files: data.ss // control.ss
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
2 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
1 #_Nblock_Patterns
 1 #_blocks_per_pattern 
# begin and end years of blocks
 2014 2023
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 0 1 0 0 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: null;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  21-24 keep last dev for rest of years
#
#
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
3 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
2 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
3 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms																	
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn			
#	Sex:	1	BioPattern:	1	NatMort												
	0.01	1	0.085	0.085	0.03	6	3	0	0	0	0	0	1	2	#	NatM_p_1_Fem_GP_1	
#	Sex:	1	BioPattern:	1	Growth												
	1	45	26.858	0	0.2	0	2	0	0	0	0	0	0	0	#	L_at_Amin_Fem_GP_1	
	10	100	52.1702	0	0.2	0	3	0	0	0	0	0	0	0	#	L_at_Amax_Fem_GP_1	
	0.01	2	0.110543	0	0.8	0	2	0	0	0	0	0	0	0	#	VonBert_K_Fem_GP_1	
	0.0001	3	0.149679	2	0.8	0	3	0	0	0	0	0	0	0	#	CV_young_Fem_GP_1	
	0.0001	3	0.106165	4	0.8	0	4	0	0	0	0	0	0	0	#	CV_old_Fem_GP_1	
#	Sex:	1	BioPattern:	1	WtLen												
	0	1	2.90E-06	0.00428	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Fem_GP_1	
	2.5	4.5	3.3369	3.2298	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Fem_GP_1	
#	Sex:	1	BioPattern:	1	Maturity&Fecundity												
	3	100	12.47	33.3	0.8	0	-3	0	0	0	0	0	0	0	#	Mat50%_Fem_GP_1	
	-3	3	-0.363	-0.52	0.8	0	-3	0	0	0	0	0	0	0	#	Mat_slope_Fem_GP_1	
	-3	3	1	1	0.8	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_inter_Fem_GP_1	
	-3	4	0	0	0.8	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_slope_wt_Fem_GP_1	
#	Sex:	2	BioPattern:	1	NatMort												
	0.01	1	0.085	0.085	0.03	6	3	0	0	0	0	0	1	2	#	NatM_p_1_Mal_GP_1	
#	Sex:	2	BioPattern:	1	Growth												
	1	45	28.4907	10.17	0.8	0	2	0	0	0	0	0	0	0	#	L_at_Amin_Mal_GP_1	
	20	70	44.0238	37.36	0.8	0	3	0	0	0	0	0	0	0	#	L_at_Amax_Mal_GP_1	
	0.05	0.35	0.151851	0.204	0.8	0	2	0	0	0	0	0	0	0	#	VonBert_K_Mal_GP_1	
	0.0001	3	0.152772	2	0.8	0	3	0	0	0	0	0	0	0	#	CV_young_Mal_GP_1	
	0.0001	3	0.0891201	4	0.8	0	4	0	0	0	0	0	0	0	#	CV_old_Mal_GP_1	
#	Sex:	2	BioPattern:	1	WtLen												
	0	1	2.90E-06	0.00428	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Mal_GP_1	
	2.5	4.5	3.3369	3.2298	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Mal_GP_1	
# Hermaphroditism
#  Recruitment Distribution  
 -4 4 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_GP_1
 -4 4 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_Area_1
 -4 4 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_month_1
#  Cohort growth dev base
 -4 4 1 1 0 0 -4 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 1e-06 0.999999 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_timevary MG parameters
#LO	HI	INIT	Value	SD	Type	Phase	Parameter	Label
#COND:	Only	if	MG	parameters	are	time-varying		
0.01	1	0.085	0.085	0.03	6	4	#NatMort_Female	
0.01	1	0.085	0.085	0.03	6	5	#NatMort_Male	

#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
       4.04039       14.0404       9.87073            11            10             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1             1             1          0.05             0         -4          0          0          0          0          0          0          0 # SR_BH_steep
             0             2      0.487182           0.6           0.8             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             1             0         -4          0          0          0          0          0          0          0 # SR_regime
             0             0             0             0             0             0        -99          0          0          0          0          0          0          0 # SR_autocorr
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1978 # first year of main recr_devs; early devs can preceed this era
2020 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 6 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
1976.7   #_last_early_yr_nobias_adj_in_MPD 
1987.3   #_first_yr_fullbias_adj_in_MPD 
2009.9   #_last_yr_fullbias_adj_in_MPD 
2020.0   #_first_recent_yr_nobias_adj_in_MPD 
0.621   #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)   
 0 #_period of cycles in recruitment (N parms read below)
 -4 #min rec_dev
 4 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1947E 1948E 1949E 1950E 1951E 1952E 1953E 1954E 1955E 1956E 1957E 1958E 1959E 1960E 1961E 1962E 1963E 1964E 1965E 1966E 1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978E 1979E 1980E 1981E 1982E 1983E 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014F 2015F 2016F
#  -0.105928 -0.111541 -0.116198 -0.120958 -0.138531 -0.135714 -0.129869 -0.140019 -0.118871 -0.0871918 0.0102857 0.0844634 0.174144 0.271812 0.359951 0.407924 0.395765 0.341229 0.279395 0.230328 0.188959 0.145264 0.0969221 0.0483237 0.00470372 -0.0281555 -0.0397376 -0.0139934 0.0673002 0.229247 0.45314 0.418412 0.20685 0.162262 0.197049 0.304032 0.688767 0.448845 0.01403 0.422116 0.00517206 -0.0253935 -0.215549 -0.305005 0.109061 -0.295284 -0.103988 0.103188 0.179318 0.13282 -0.558396 -0.489532 1.37383 0.119574 0.638243 -0.583553 -0.0365912 0.118737 0.0566511 -0.501263 -0.234672 -0.673581 -0.379689 -0.349194 0.760021 0.935015 -0.66493 -0.731386 -0.00134698 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0 # F ballpark
-1984 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 1
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
# 0 2 0.0056134 0 99 0 1 # InitF_seas_1_flt_1Fishery
# 0 2 0 0 99 0 -1 # InitF_seas_1_flt_1Fishery

#2016 2036
# F rates by fleet
# Yr:  1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.00779872 0.00500123 0.00537229 0.00429513 0.00427666 0.00329377 0.00121876 0.000393253 0.000207785 0.00049934 0.00960522 0.0133719 0.0206498 0.0883093 0.0800805 0.0377758 0.03145 0.0215364 0.0226936 0.0389011 0.0242874 0.0251377 0.010765 0.00901846 0.00620067 0.0104052 0.00744812 0.00439444 0.00413385 0.00297721 0.00582953 0.00476126 0.0055031 0.00470737 0.00264765 0.00245913 0.00330826 0.00224572 0
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         0  #  Survey1
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	-1	1	-0.1204130	0.171799373	0.145066767	6	-2	0	0	0	0	0	1	2	#	Survey1000
#timevary Q parameters
#LO	HI	INIT	PRIOR	        PR_SD	        PRIOR_TYPE	PHASE	LABEL				
-1	1	0	0.171799373	0.145066767	6	        4	#LnQ	2014-2019 (replace)
#-0.99	0.99	0	0	0.5	0	-6	#LnQ	base	Survey	dev (only use this line if doing deviations)	

#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 0 3 0 # 1 Fishery
 0 0 0 0 # 2 Survey1
 0 0 0 0 # 3 Survey2
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#_Pattern Discard Male Special
 0 0 0 0 # 1 Fishery
 20 0 3 0 # 2 Survey1
 20 0 3 0 # 3 Survey2
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Fishery LenSelex
             7            80       48.7906            15             5             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_Fishery(1)
            -5             3             0             0             5             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Fishery(1)
           -10            10       4.25427           3.2             5             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Fishery(1)
           -10            10            10           3.4             5             0         -2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Fishery(1)
           -30             5           -10          -999             5             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Fishery(1)
            -5             5           999           999             5             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_Fishery(1)
           -30            30      -9.27296             0             5             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_Fishery(1)
           -15            15      -1.45169             0             5             0          4          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_Fishery(1)
           -15            15             0             0             5             0         -4          0          0          0          0          0          0          0  #  SzSel_Male_Descend_Fishery(1)
             0             1             0             0             5             0         -4          0          0          0          0          0          0          0  #  SzSel_Male_Final_Fishery(1)
             0             1             1             0             5             0         -4          0          0          0          0          0          0          0  #  SzSel_Male_Scale_Fishery(1)
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name		
#Survey 1	AgeSelex																	
	1	45	3.2	11	5	0	2	0	0	0	0	0	0	0	#	Age_DblN_peak_Survey1(5)		
	-5	50	8	30	5	0	-3	0	0	0	0	0	0	0	#	Age_DblN_top_logit_Survey1(5)		
	-4	12	2.3	4	5	0	2	0	0	0	0	0	0	0	#	Age_DblN_ascend_se_Survey1(5)		
	-2	15	15	3.4	5	0	-4	0	0	0	0	0	0	0	#	Age_DblN_descend_se_Survey1(5)		
	-999	1	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	Age_DblN_start_logit_Survey1(5)		
	-999	5	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	Age_DblN_end_logit_Survey1(5)		
	-15	15	0	0	5	0	1	0	0	0	0	0	0	0	#	AgeSel_2Male_Peak_Survey1		
	-15	15	0	0	5	0	2	0	0	0	0	0	0	0	#	AgeSel_2Male_Ascend_Survey1		
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_2Male_Descend_Survey1		
	-999	999	-999	0	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_2Male_Final_Survey1		
	-15	15	1	0	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_2Male_Scale_Survey1		
#Survey2	AgeSelex																
	1	100	23.5579	11	5	0	1	0	0	0	0	0	0	0	#	Age_DblN_peak_Survey2(6)	
	-5	15	-0.303601	30	5	0	2	0	0	0	0	0	0	0	#	Age_DblN_top_logit_Survey2(6)	
	-4	12	5.09699	4.1	5	0	2	0	0	0	0	0	0	0	#	Age_DblN_ascend_se_Survey2(6)	
	-2	15	15	3.4	5	0	-2	0	0	0	0	0	0	0	#	Age_DblN_descend_se_Survey2(6)	
	-999	1	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	Age_DblN_start_logit_Survey2(6)	
	-999	5	-999	999	5	0	-2	0	0	0	0	0	0	0	#	Age_DblN_end_logit_Survey2(6)	
	-30	15	0	0	5	0	2	0	0	0	0	0	0	0	#	AgeSel_3Male_Peak_Survey2	
	-15	15	0	0	5	0	3	0	0	0	0	0	0	0	#	AgeSel_3Male_Ascend_Survey2	
	-15	5	0	0	5	0	3	0	0	0	0	0	0	0	#	AgeSel_3Male_Descend_Survey2	
	-999	0	-999	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Final_Survey2	
	0	1	1	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Scale_Survey2	
#_no timevary selex parameters
##_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	#	parm_name		
##	1978	to	2013								
#	1	45	3.2	11	5	0	2	#	Age_DblN_peak_Survey1(5)		
#	-4	12	2.3	4	5	0	2	#	Age_DblN_ascend_se_Survey1(5)		
#	-2	15	15	3.4	5	0	4	#	Age_DblN_descend_se_Survey1(5)		
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#Data_type Fleet New_Var_adj hash Old_Var_adj New_Francis New_MI Francis_mult Francis_lo Francis_hi MI_mult Type Name Note
4 1 0.29 # 0.3 0.29 0.69 0.96 0.69 1.67 2.31 len Fishery 
4 2 0.13 # 0.1 0.13 1.56 1.33 0.93 3.06 15.63 len Survey1 
4 3 0.13 # 0.1 2 0.46 20.03 15.39 Inf 4.62 len Survey2 
5 2 0.49 # 0.49 0.49 0.42 0.99 0.64 2.99 0.85 age Survey1 
5 3 0.49 # 0.49 2.11 0.4 4.3 4.3 Inf 0.82 age Survey2 
 -9999   1    0  # terminator
#
10 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 5 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
 4 1 1 1 1
 4 2 1 1 1
 4 3 1 1 1
 5 2 1 1 1
 5 3 1 1 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 0 0 0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 1 1 1 1 1 1 #_CPUE/survey:_2
#  0 0 0 0 0 0 0 0 0 0 #_CPUE/survey:_3
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_3
#  0 0 0 0 0 0 0 0 0 0 #_agecomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_3
#  1 1 1 1 1 1 1 1 1 1 #_init_equ_catch
#  1 1 1 1 1 1 1 1 1 1 #_recruitments
#  1 1 1 1 1 1 1 1 1 1 #_parameter-priors
#  1 1 1 1 1 1 1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 1 1 1 1 1 1 #_crashPenLambda
#  0 0 0 0 0 0 0 0 0 0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 0 0 0 0 0 0 0 0 # placeholder for # selex_fleet, 1=len/2=age/3=both, year, N selex bins, 0 or Growth pattern, N growth ages, 0 or NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

