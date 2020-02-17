
# labels
HIV_labs = c("S_lo", "S_hi", "S_pr",
             # "I_lo_new", "I_lo_mid", "I_lo_old",
             # "I_hi_new", "I_hi_mid", "I_hi_old",
             # "I_pr_new", "I_pr_mid", "I_pr_old",
             "I_lo_new", "I_lo_old",
             "I_hi_new", "I_hi_old",
             "I_pr_new", "I_pr_old",
             "D1_lo", "D2_lo", "D3_lo",
             "D1_hi", "D2_hi", "D3_hi",
             "D1_pr", "D2_pr", "D3_pr")
             # "D1", "D2", "D3")
STI_labs = c("S", "E", "I_s", "I_a", "T")
med_labs = c("aus", "int")

# secondary labels
HIV_risk_labs = c("lo", "hi", "pr")
timeindex = c("new", "old")

# compartment array
# dim 1: HIV status -- only this one is in use at the moment
# dim 2: STI status
# dim 3: medicare status
SID_mat = makearray(list(HIV_labs, STI_labs, med_labs))

HIV_labs_get = function(x) grep(x, HIV_labs, value=TRUE)
HIV_risk_index = setNames(ifelse(grepl('lo', HIV_labs), 'lo', ifelse(grepl('hi', HIV_labs), 'hi', ifelse(grepl('pr', HIV_labs), 'pr', 'na'))),
                          HIV_labs)

# HIV compartment label index
sHIV = list()
sHIV[['S']] = HIV_labs_get("^S.*")
sHIV[['I']] = HIV_labs_get("^I.*")
sHIV[['D']] = HIV_labs_get("^D.*")
sHIV[['I_new']] = HIV_labs_get("I_.._new")
sHIV[['I_lo']] = HIV_labs_get("^I_lo.*")
sHIV[['I_hi']] = HIV_labs_get("^I_hi.*")
sHIV[['I_pr']] = HIV_labs_get("^I_pr.*")
sHIV[['D1']] = HIV_labs_get("D1")
sHIV[['D2']] = HIV_labs_get("D2")
sHIV[['D3']] = HIV_labs_get("D3")
sHIV[['D1plus']] = unique(c(sHIV$D1, sHIV$D2, sHIV$D3))
sHIV[['D2plus']] = unique(c(sHIV$D2, sHIV$D3))
sHIV[['D3plus']] = sHIV$D3
sHIV[['D_lo']] = HIV_labs_get("^D._lo")
sHIV[['D_hi']] = HIV_labs_get("^D._hi")
sHIV[['D_pr']] = HIV_labs_get("^D._pr")
sHIV[['D1_lo']] = HIV_labs_get("^D1_lo")
sHIV[['D2_lo']] = HIV_labs_get("^D2_lo")
sHIV[['D3_lo']] = HIV_labs_get("^D3_lo")
sHIV[['D1_hi']] = HIV_labs_get("^D1_hi")
sHIV[['D2_hi']] = HIV_labs_get("^D2_hi")
sHIV[['D3_hi']] = HIV_labs_get("^D3_hi")
sHIV[['D1_pr']] = HIV_labs_get("^D1_pr")
sHIV[['D2_pr']] = HIV_labs_get("^D2_pr")
sHIV[['D3_pr']] = HIV_labs_get("^D3_pr")
sHIV[['eff_I_lo']] = union(sHIV$I_lo, sHIV$D_lo)
sHIV[['eff_I_hi']] = union(sHIV$I_hi, sHIV$D_hi)
sHIV[['eff_I_pr']] = union(sHIV$I_pr, sHIV$D_pr)
sHIV[['PLHIV']] = union(sHIV[['I']], sHIV[['D']])
sHIV[['lo']] = HIV_labs_get("lo")
sHIV[['hi']] = HIV_labs_get("hi")
sHIV[['pr']] = HIV_labs_get("pr")
sHIV[['und']] = setdiff(HIV_labs, sHIV$D)
sHIV[['S_lo']] = 'S_lo'
sHIV[['S_hi']] = 'S_hi'
sHIV[['S_pr']] = 'S_pr'

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
                        
                        c("I_lo_new_d_aus", "I_lo_new", "D1_lo", 1),
                        c("I_lo_old_d_aus", "I_lo_old", "D1_lo", 1),
                        c("I_hi_new_d_aus", "I_hi_new", "D1_hi", 1),
                        c("I_hi_old_d_aus", "I_hi_old", "D1_hi", 1),
                        c("I_pr_new_d_aus", "I_pr_new", "D1_pr", 1),
                        c("I_pr_old_d_aus", "I_pr_old", "D1_pr", 1),
                        
                        c("I_lo_new_d_int", "I_lo_new", "D1_lo", 2),
                        c("I_lo_old_d_int", "I_lo_old", "D1_lo", 2),
                        c("I_hi_new_d_int", "I_hi_new", "D1_hi", 2),
                        c("I_hi_old_d_int", "I_hi_old", "D1_hi", 2),
                        c("I_pr_new_d_int", "I_pr_new", "D1_pr", 2),
                        c("I_pr_old_d_int", "I_pr_old", "D1_pr", 2),
                        
                        c("treat_lo", "D1_lo", "D2_lo", 0),
                        c("treat_hi", "D1_hi", "D2_hi", 0),
                        c("treat_pr", "D1_pr", "D2_pr", 0),
                        c("viral_supp_lo", "D2_lo", "D3_lo", 0),
                        c("viral_supp_hi", "D2_hi", "D3_hi", 0),
                        c("viral_supp_pr", "D2_pr", "D3_pr", 0),
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
tHIV[['test_lo']] = grep("I_lo_..._d", HIV_transitions, value=TRUE)
tHIV[['test_hi']] = grep("I_hi_..._d", HIV_transitions, value=TRUE)
tHIV[['test_pr']] = grep("I_pr_..._d", HIV_transitions, value=TRUE)


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
