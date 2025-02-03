
################################## LIBRARY ############################

library(irr)
library(psych)

############################# OFT ########################

# Expl dur ---------------------------------------------------

OFT_expl_dur <- matrix(c(
  67.012,92.727,
  51.496,57.872,
  37.733,21.086,
  20.996,26.487,
  64.440,52.357,
  49.940,46.041,
  44.949,46.938,
  40.693,35.395,
  34.443,36.232,
  22.504,17.729
),ncol=2,byrow=TRUE)

icc(OFT_expl_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(OFT_expl_dur)

# Locom dur --------------------------------------------------

OFT_locom_dur <- matrix(c(
  129.389,	136.464,
  168.670,	170.003,
  105.954,	123.414,
  63.978,	69.482,
  84.555,	96.566,
  81.385,	92.026,
  108.569,	112.055,
  112.981,	110.603
),ncol=2,byrow=TRUE)

icc(OFT_locom_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(OFT_locom_dur)

# Voc freq ---------------------------------------------------

OFT_voc_freq <- matrix(c(
  183,193,
  192,211,
  250,250,
  195,203,
  167,169,
  187,188,
  326,312,
  225,232
),ncol=2,byrow=TRUE)

icc(OFT_voc_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(OFT_voc_freq)

# Jump freq --------------------------------------------------

OFT_jump_freq <- matrix(c(
  2, 2,
  2, 2,
  0, 0,
  3, 3,
  0, 0,
  0, 0,
  0, 0,
  0, 0
),ncol=2,byrow=TRUE)

icc(OFT_jump_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(OFT_jump_freq)

#without zeros
OFT_jump_freq <- matrix(c(
  2,	2,
  2,	2,
  3,	3,
),ncol=2,byrow=TRUE)

icc(OFT_jump_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(OFT_jump_freq)

############################# NOT ########################

# Expl obj dur ---------------------------------------------

NOT_expl_obj_dur <- matrix(c(
  26.995,28.094,
  82.839,87.050,
  13.194,9.394,
  22.437,22.792
),ncol=2,byrow=TRUE)

icc(NOT_expl_obj_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NOT_expl_obj_dur)

# Expl obj freq --------------------------------------------

NOT_expl_obj_freq <- matrix(c(
  6,	5,
  8,	7,
  4,	2,
  7,	7
),ncol=2,byrow=TRUE)

icc(NOT_expl_obj_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NOT_expl_obj_freq)

# Expl obj lat --------------------------------------------

NOT_expl_obj_lat <- matrix(c(
  9.832,	9.920,
  0.935,	1.434,
  25.116,	26.116,
  21.969,	19.120
),ncol=2,byrow=TRUE)

icc(NOT_expl_obj_lat, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NOT_expl_obj_lat)

# Inter voc dur --------------------------------------------

NOT_inter_voc_dur <- matrix(c(
  15.414,	15.765,
  8.236,	8.481,
  3.003,	3.107,
  12.628,	13.131,
  9.132,	9.084,
  5.785,	5.485,
  10.117,	10.366,
  13.621,	13.420
),ncol=2,byrow=TRUE)

icc(NOT_inter_voc_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NOT_inter_voc_dur)

############################# HAT ########################

# Expl hum lat ---------------------------------

HAT_expl_hum_lat <- matrix(c(
  6.169,	5.768,
  46.021,	45.521,
  57.200,	56.952,
  81.453,	81.306,
  55.086,	55.290,
  73.920,	73.574,
  81.194,	80.944,
  112.091,	112.792
),ncol=2,byrow=TRUE)

icc(HAT_expl_hum_lat, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(HAT_expl_hum_lat)

# Expl hum dur ---------------------------------

HAT_expl_hum_dur <- matrix(c(
  29.297,20.297,
  41.739,42.493,
  23.851,20.342,
  23.040,23.532,
  10.495,11.399,
  1.251,1.049,
  6.492,7.249,
  53.450,49.300
),ncol=2,byrow=TRUE)

icc(HAT_expl_hum_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(HAT_expl_hum_dur)

# Jump freq ---------------------------------

HAT_jump_freq <- matrix(c(
  1,	1,
  0, 0,
  0, 0,
  0, 0,
  1,	1,
  0, 0
),ncol=2,byrow=TRUE)

icc(HAT_jump_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(HAT_jump_freq)

#without zeros
HAT_jump_freq <- matrix(c(
  1,	1,
  1,	1
),ncol=2,byrow=TRUE)

icc(HAT_jump_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(HAT_jump_freq)

############################# NPT ########################

# Back dur ----------------------------------------------------------

NPT_back_dur <- matrix(c(
  5.249,	5.501,
  151.749,	143.737,
  40.495,	42.742
),ncol=2,byrow=TRUE)

icc(NPT_back_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_back_dur)

# Facing fence dur ----------------------------------------------------------

NPT_facing_fence_dur <- matrix(c(
  261.500,	238.244,
  259.997,	237.965,
  203.489,	228.498,
  199.584,	184.317,
  151.749,	143.737,
  177.985,	158.485
),ncol=2,byrow=TRUE)

icc(NPT_facing_fence_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_facing_fence_dur)

# Facing fence dur ----------------------------------------------------------

NPT_facing_fence_freq <- matrix(c(
  2,	4,
  2,	5,
  6,	6,
  12,	12,
  14,	13,
  13,	13
),ncol=2,byrow=TRUE)

icc(NPT_facing_fence_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_facing_fence_freq)

# Front freq ----------------------------------------------------------

NPT_front_freq <- matrix(c(
  1,	1,
  1,	1,
  1,	1,
  2,	2,
  1,	1,
  5,	5
),ncol=2,byrow=TRUE)

icc(NPT_front_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_front_freq)

# Middle dur ----------------------------------------------------------

NPT_middle_dur <- matrix(c(
  21.089,	21.085,
  0, 0,
  0, 0,
  16.248,	15.745,
  0, 0,
  37.999,	36.746
),ncol=2,byrow=TRUE)

icc(NPT_middle_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_middle_dur)

#without zeros
NPT_middle_dur <- matrix(c(
  21.089,	21.085,
  16.248,	15.745,
  37.999,	36.746
),ncol=2,byrow=TRUE)

icc(NPT_middle_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_middle_dur)

# Sudden display dur ----------------------------------------------------------

NPT_sudden_display_dur <- matrix(c(
  0, 0,
  4.249, 3.998,
  0, 0,
  0, 0,
  0, 0,
  0, 0
),ncol=2,byrow=TRUE)

icc(NPT_sudden_display_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_sudden_display_dur)

# Front freq ----------------------------------------------------------

NPT_turn_back_freq <- matrix(c(
  0, 0,
  0, 0,
  0, 0,
  1,	1,
  0, 0,
  3,	3
),ncol=2,byrow=TRUE)

icc(NPT_turn_back_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_turn_back_freq)

#without zeros
NPT_turn_back_freq <- matrix(c(
  1,	1,
  3,	3
),ncol=2,byrow=TRUE)

icc(NPT_turn_back_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_turn_back_freq)

# Walk fence dur --------------------------------------------------------

NPT_walk_fence_dur <- matrix(c(
  8.498,	31.750,
  32.026,	36.752,
  66.501,	41.492,
  74.662,	61.646,
  118.225,	126.239,
  51.496,	68.749
),ncol=2,byrow=TRUE)

icc(NPT_walk_fence_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_walk_fence_dur)

# Nose nose lat -----------------------------------------------

NPT_nose_nose_lat <- matrix(c(
  29.716,	30.212,
  0.430,	0.927,
  11.327,	11.074,
  53.823,	53.573,
  5.479,	5.479
),ncol=2,byrow=TRUE)

icc(NPT_nose_nose_lat, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_nose_nose_lat)

# Nose nose freq -----------------------------------------------

NPT_nose_nose_freq <- matrix(c(
  27,35,
  30,24,
  38,46,
  30,25,
  22,21,
  3,3
),ncol=2,byrow=TRUE)

icc(NPT_nose_nose_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_nose_nose_freq)

# Tail wag freq ----------------------------------------------------------

NPT_tail_wag_freq <- matrix(c(
  4,3,
  0,0,
  0,0,
  5,5,
  3,3,
  0,0,
  23,17,
  1,1,
  29,25,
  2,2,
  10,9
),ncol=2,byrow=TRUE)

icc(NPT_tail_wag_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(NPT_tail_wag_freq)

########################### BIBAGO ######################

# Voc freq ----------------------------------------------------------

BIBAGO_voc_freq <- matrix(c(
  
),ncol=2,byrow=TRUE)

icc(BIBAGO_voc_freq, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_voc_freq)

# Inter voc dur ----------------------------------------------------------

BIBAGO_inter_voc_dur <- matrix(c(
  4.874,	0.001,
  0.379,	0.430,
  0.001,	1.299,
  0.001, 0.001,
  0.001,	8.920,
  83.280,	83.485,
  3.180,	3.630,
  62.244,	62.498,
  0.001,	3.639,
  1.006,	0.001,
  6.253,	6.653,
  0.001, 0.001,
  1.886,	2.034,
  53.904,	54.004,
  1.487,	1.685,
  0.001, 0.001
),ncol=2,byrow=TRUE)

icc(BIBAGO_inter_voc_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_inter_voc_dur)

#without zeros
BIBAGO_inter_voc_dur <- matrix(c(
  4.874,	0.001,
  0.379,	0.430,
  0.001,	1.299,
  0.001,	8.920,
  83.280,	83.485,
  3.180,	3.630,
  62.244,	62.498,
  0.001,	3.639,
  1.006,	0.001,
  6.253,	6.653,
  1.886,	2.034,
  53.904,	54.004,
  1.487,	1.685
),ncol=2,byrow=TRUE)

icc(BIBAGO_inter_voc_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_inter_voc_dur)

#  Chewing dur ----------------------------------------------------------

BIBAGO_chewing_dur <- matrix(c(
  29.040,	36.488,
  50.899,	40.404,
  93.268,	114.867,
  26.748,	32.956,
  25.499,	27.302,
  0, 0,
  11.751,	7.997,
  65.008,	90.067,
  0, 0,
  75.903,	86.548,
  28.348,	36.908,
  0, 0
),ncol=2,byrow=TRUE)

icc(BIBAGO_chewing_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_chewing_dur)

#without zeros
BIBAGO_chewing_dur <- matrix(c(
  29.040,	36.488,
  50.899,	40.404,
  93.268,	114.867,
  26.748,	32.956,
  25.499,	27.302,
  11.751,	7.997,
  65.008,	90.067,
  75.903,	86.548,
  28.348,	36.908
),ncol=2,byrow=TRUE)

icc(BIBAGO_chewing_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_chewing_dur)

# Expl wob lat ----------------------------------------------------------

BIBAGO_expl_wob_lat <- matrix(c(
  68.228,	68.978,
  1.132,	0.731,
  48.338,	48.101,
  63.637,	64.182,
  6.407,	7.270,
  3.369,	3.234,
  37.373,	37.883,
  3.196,	3.047,
  3.600,	3.761,
  56.840,	56.653,
  2.440,	2.954,
  2.120,	2.819,
  17.960,	17.934,
  96.280,	96.654,
  45.040,	45.086,
  115.200,	115.278
),ncol=2,byrow=TRUE)

icc(BIBAGO_expl_wob_lat, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_expl_wob_lat)

# Expl wob dur ----------------------------------------------------------

BIBAGO_expl_wob_dur <- matrix(c(
  12.009,	12.596,
  68.093,	57.837,
  104.414,	115.934,
  36.144,	19.100,
  9.901,	9.901,
  5.844,	5.844,
  6.793,	6.793,
  98.245,	98.245,
  3.354,	3.354,
  10.051,	10.051,
  0.651,	0.651,
  3.899,	3.899,
  6.495,	6.928,
  65.682,	64.794,
  22.749,	21.892,
  6.091,	5.902
),ncol=2,byrow=TRUE)

icc(BIBAGO_expl_wob_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_expl_wob_dur)

# Freez dur ----------------------------------------------------------

BIBAGO_freez_dur <- matrix(c(
  35.695,	68.103,
  3.997,	17.255,
  20.492,	71.406,
  36.745,	80.973,
  25.249,	28.758,
  8.656,	8.501,
  9.002,	6.499,
  53.492,	33.300,
  19.703,	12.997,
  8.247,	9.253,
  11.251,	13.299,
  0,	3.25,
  12.9,	6.752,
  0,	0,
  27.51,	24.249,
  74.659,	68.698
),ncol=2,byrow=TRUE)

icc(BIBAGO_freez_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_freez_dur)

#without zeros
BIBAGO_freez_dur <- matrix(c(
  35.695,	68.103,
  3.997,	17.255,
  20.492,	71.406,
  36.745,	80.973,
  25.249,	28.758,
  8.656,	8.501,
  9.002,	6.499,
  53.492,	33.300,
  19.703,	12.997,
  8.247,	9.253,
  11.251,	13.299,
  0,	3.25,
  12.9,	6.752,
  27.51,	24.249,
  74.659,	68.698
),ncol=2,byrow=TRUE)

icc(BIBAGO_freez_dur, model = "twoway", 
    type = "agreement", 
    unit = "single", 
    conf.level = 0.95)

ICC(BIBAGO_freez_dur)
