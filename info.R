
# labels
HIV_labs = c("S_lo", "S_hi", "S_pr",
             # "I_lo_new", "I_lo_mid", "I_lo_old",
             # "I_hi_new", "I_hi_mid", "I_hi_old",
             # "I_pr_new", "I_pr_mid", "I_pr_old",
             "I_lo_new", "I_lo_old",
             "I_hi_new", "I_hi_old",
             "I_pr_new", "I_pr_old",
             "D1", "D2", "D3")
STI_labs = c("S", "E", "I_s", "I_a", "T")
med_labs = c("aus", "int")

# compartment array
# dim 1: HIV status -- only this one is in use at the moment
# dim 2: STI status
# dim 3: medicare status
SID_mat = array(0, dim = c(length(HIV_labs), length(STI_labs), length(med_labs)), dimnames = list(HIV_labs, STI_labs, med_labs))

# HIV compartment label index
sHIV = list()
sHIV[['S']] = grep("^S.*", HIV_labs, value=TRUE)
sHIV[['I']] = grep("^I.*", HIV_labs, value=TRUE)
sHIV[['D']] = grep("^D.*", HIV_labs, value=TRUE)
sHIV[['I_new']] = grep("I_.._new", HIV_labs, value=TRUE)
sHIV[['I_lo']] = grep("^I_lo.*", HIV_labs, value=TRUE)
sHIV[['I_hi']] = grep("^I_hi.*", HIV_labs, value=TRUE)
sHIV[['I_pr']] = grep("^I_pr.*", HIV_labs, value=TRUE)
# sHIV[['eff_I_lo']] = union(sHIV[['I_lo']], sHIV[['D']])
# sHIV[['eff_I_hi']] = sHIV[['I_hi']]
# sHIV[['eff_I_pr']] = sHIV[['I_pr']]
sHIV[['D1']] = c("D1")
sHIV[['D2']] = c("D2")
sHIV[['D3']] = c("D3")
sHIV[['D1plus']] = unique(c(sHIV$D1, sHIV$D2, sHIV$D3))
sHIV[['D2plus']] = unique(c(sHIV$D2, sHIV$D3))
sHIV[['D3plus']] = sHIV$D3
sHIV[['PLHIV']] = union(sHIV[['I']], sHIV[['D']])
sHIV[['lo']] = grep("lo", HIV_labs, value=TRUE)
sHIV[['hi']] = grep("hi", HIV_labs, value=TRUE)
sHIV[['pr']] = grep("pr", HIV_labs, value=TRUE)
sHIV[['und']] = setdiff(HIV_labs, sHIV$D)
sHIV[['S_pr']] = 'S_pr'
sHIV[['S_lo']] = 'S_lo'
sHIV[['S_hi']] = 'S_hi'

# HIV transitions -- e.g. infection, diagnosis, starting treatment, etc.
HIV_transitions = rbind(c("S_lo_inf_aus", "S_lo", "I_lo_new", 1),
                        c("S_hi_inf_aus", "S_hi", "I_hi_new", 1),
                        c("S_pr_inf_aus","S_pr", "I_pr_new", 1),
                        c("S_lo_inf_int", "S_lo", "I_lo_new", 2),
                        c("S_hi_inf_int", "S_hi", "I_hi_new", 2),
                        c("S_pr_inf_int","S_pr", "I_pr_new", 2),
                        # c("I_lo_wait_1", "I_lo_new", "I_lo_mid", 0),
                        # c("I_lo_wait_2","I_lo_mid", "I_lo_old", 0),
                        # c("I_hi_wait_1", "I_hi_new", "I_hi_mid", 0),
                        # c("I_hi_wait_2", "I_hi_mid", "I_hi_old", 0),
                        # c("I_pr_wait_1", "I_pr_new", "I_pr_mid", 0),
                        # c("I_pr_wait_2", "I_pr_mid", "I_pr_old", 0),
                        c("I_lo_wait_1", "I_lo_new", "I_lo_old", 0),
                        c("I_hi_wait_1", "I_hi_new", "I_hi_old", 0),
                        c("I_pr_wait_1", "I_pr_new", "I_pr_old", 0),
                        c("I_lo_new_d", "I_lo_new", "D1", 0),
                        # c("I_lo_mid_d", "I_lo_mid", "D1", 0),
                        c("I_lo_old_d", "I_lo_old", "D1", 0),
                        c("I_hi_new_d", "I_hi_new", "D1", 0),
                        # c("I_hi_mid_d", "I_hi_mid", "D1", 0),
                        c("I_hi_old_d", "I_hi_old", "D1", 0),
                        c("I_pr_new_d", "I_pr_new", "D1", 0),
                        # c("I_pr_mid_d", "I_pr_mid", "D1", 0),
                        c("I_pr_old_d", "I_pr_old", "D1", 0),
                        c("treat", "D1", "D2", 0),
                        c("viral_supp", "D2", "D3", 0),
                        c("become_high_risk", "S_lo", "S_hi", 1),
                        c("start_prep_aus", "S_hi", "S_pr", 1),
                        c("start_prep_int", "S_hi", "S_pr", 2)
                        
)
colnames(HIV_transitions) = c("trans", "from", "to", "med")

# HIV transition label index
tHIV = list()
tHIV[['inf']] = grep("inf", HIV_transitions, value=TRUE)
tHIV[['wait_1']] = grep("wait_1", HIV_transitions, value=TRUE)
tHIV[['wait_2']] = grep("wait_2", HIV_transitions, value=TRUE)
tHIV[['wait']] = grep("wait", HIV_transitions, value=TRUE)
tHIV[['test_new']] = grep("I_.._new_d", HIV_transitions, value=TRUE)
# tHIV[['test_mid']] = grep("I_.._mid_d", HIV_transitions, value=TRUE)
tHIV[['test_old']] = grep("I_.._old_d", HIV_transitions, value=TRUE)
tHIV[['test']] = grep("I_.._..._d", HIV_transitions, value=TRUE)


# STI compartment label index
sSTI = list()
sSTI[['I']] = grep("I", STI_labs, value=TRUE)

STI_transitions = rbind(c('exp', 'S', 'E'),
                        c('inf_s', 'E', 'I_s'),
                        c('inf_a', 'E', 'I_a'),
                        c('treat_s', 'I_s', 'T'),
                        c('treat_a', 'I_a', 'T'),
                        c('recov', 'T', 'S')
)
colnames(STI_transitions) = c("trans", "from", "to")
