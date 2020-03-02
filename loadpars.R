### Loading Parameters and Data ###

# import data

data_raw = read_excel("data_sti.xlsx", sheet="data")

# create matrices from data and omit absent values,

data_years = data_raw[,1]
data_cols = colnames(data_raw)

PLHIV0 =  data_raw[,c(1,which(data_cols=="PLHIV_tot"))]
PLHIV0_index = !is.na(PLHIV0[,2]) # [1:max(which(!is.na(PLHIV0[,2])==TRUE))]
PLHIV0 = PLHIV0[!is.na(PLHIV0[,2]),]
prop_diag0 =  data_raw[,c(1,which(data_cols=="prop_HIV_diagnosed"))]
prop_diag0_index = !is.na(prop_diag0[,2]) # [1:max(which(!is.na(prop_diag0[,2])==TRUE))]
prop_diag0 = prop_diag0[!is.na(prop_diag0[,2]),]
prop_diag0 = prop_diag0[-1,]
totalpop0 =  data_raw[,c(1,which(data_cols=="pop_tot"))] 
totalpop0 = totalpop0[!is.na(totalpop0[,2]),]

risk_data_to_frame = function(this_str){
  diag_cols = which(grepl(paste0(this_str), data_cols))
  df = data_raw[,c(1,diag_cols)]
  # colnames(df) = c('t', substring(colnames(df)[-1], nchar(this_str)+2))
  dot_positions = unlist(str_locate_all(this_str, '\\.\\.\\.'))
  colnames(df) = c('t', substring(colnames(df)[-1], dot_positions[1], dot_positions[2]))
  df = reshape(data.frame(df),
               direction = 'long',
               varying = list(colnames(df)[-1]),
               v.names = 'value',
               idvar = 't',
               timevar = 'med_pop',
               times = colnames(df)[-1])
  df = df[!is.na(df$value),]
  rownames(df) = NULL
  return(df)
}

HIV_diag_dat = data.frame(risk_data_to_frame('HIV_diag_..._all'),
                       type='trans', dt=1, pid='HIV_diag_tot',
                       sti_pop='all', risk_pop='all', HIV_pop='HIV_diag', source='data', scen='')
PLHIV_dat = data.frame(risk_data_to_frame('PLHIV_...'),
                       type='pop', dt=1, pid = 'PLHIV_tot',
                       sti_pop='all', risk_pop='all', HIV_pop='PLHIV', source='data', scen='')
data_pop = data.frame(risk_data_to_frame('pop_...'))

HIV_diag_new = risk_data_to_frame('HIV_diag_new_...')
if(nrow(HIV_diag_new) > 0){
  HIV_diag_new = data.frame(HIV_diag_new,
                            diag_time = 'new',
                            type='trans', dt=1, pid='HIV_diag_new',
                            sti_pop='all', risk_pop='all', HIV_pop='HIV_diag_new', source='data', scen='')}
HIV_diag_old = risk_data_to_frame('HIV_diag_old_...')
if(nrow(HIV_diag_old) > 0){
  HIV_diag_old = data.frame(HIV_diag_old,
                            diag_time = 'old',
                            type='trans', dt=1, pid='HIV_diag_old',
                            sti_pop='all', risk_pop='all', HIV_pop='HIV_diag_old', source='data', scen='')}

# prev_sti0 = data_raw[,c(1,8,9)]
prev_sti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Gon prev", colnames(data_raw))]
prev_sti0_index = !is.na(prev_sti0[,2])
prev_sti0 = prev_sti0[prev_sti0_index[,1],]

# PLsti0 = data_raw[,c(1,12,13)]
PLsti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total Gon+ HIV", colnames(data_raw), fixed=T)]
PLsti0_index = !is.na(PLsti0[,2])
PLsti0 = PLsti0[PLsti0_index[,1],]

# diagnoses_sti0 = data_raw[,c(1,15,16)]
diagnoses_sti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total Gon cases HIV", colnames(data_raw), fixed=T)]
diagnoses_sti0_index = !is.na(diagnoses_sti0[,2])
diagnoses_sti0 = diagnoses_sti0[diagnoses_sti0_index[,1],]


prop_diag_dat = data.frame(t = prop_diag0$Year, value = prop_diag0$prop_HIV_diagnosed, type='pop', dt=1, pid='num_diag_prop',
                          sti_pop='all', risk_pop='all', HIV_pop='num_diag', source='data', scen='', plot='care_cascade')

prev_dat = data.frame(t = 2014, value = 0.07, type='pop', dt=1, pid='HIV_prev_prop', plot='HIV_prev', sti_pop='all', HIV_pop='HIV_prev',
                      med_pop = 'aus', risk_pop = 'all', source='data', scen='')

all_dat = rbind.fill(PLHIV_dat, HIV_diag_dat, prop_diag_dat)
all_dat = rbind.fill(all_dat, HIV_diag_new, HIV_diag_old)
all_dat = rbind.fill(all_dat, prev_dat)
all_dat$plot[is.na(all_dat$plot)] = all_dat$HIV_pop[is.na(all_dat$plot)]
all_dat$scen = 'data'
all_dat$scen_long = 'Data'
data_wide = widen_sources(all_dat)

# load mixing matrix
medi_states = do.call(paste0, expand.grid(c('lo', 'hi', 'pr'), '_', med_labs))
mixing_raw = read_excel('data_sti.xlsx', sheet='mixing', range=cell_limits(c(2,2), c(7,7)), col_names = medi_states)
mixing = as.matrix(mixing_raw)
rownames(mixing) = medi_states
mixing[!is.finite(suppressWarnings(as.numeric(mixing)))] = 0
class(mixing) = 'numeric'
# mixing = makearray(list(medi_states, medi_states))
# mixing[,] = 1



# import parameters, including upper and lower bounds (if they exist)
pars_raw = read_excel("data_sti.xlsx", sheet="pars", col_names=TRUE)

static_pars = list()
for(i in 1:nrow(pars_raw)){
  thisname = paste0(pars_raw[i,2])
  thisval = as.numeric(pars_raw[i,3])
  
  if(!is.na(pars_raw[i,4]))
  {
    thislb = as.numeric(pars_raw[i,4])
    thisub = as.numeric(pars_raw[i,5])

  } else {
    thislb = thisval
    thisub = thisval
  }
  thispar = list('name' = thisname,
                 'v' = thisval,
                 'lb' = thislb,
                 'ub' = thisub)
  static_pars[[thisname]] = thispar
}


