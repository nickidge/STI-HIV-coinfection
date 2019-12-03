HIV_labs = c("S_lo", "S_hi", "S_pr",
             "I_lo_new", "I_lo_mid", "I_lo_old",
             "I_hi_new", "I_hi_mid", "I_hi_old",
             "I_pr_new", "I_pr_mid", "I_pr_old",
             "D1", "D2", "D3")
STI_labs = c("S", "E", "Sy", "ASy", "T")
RISK_labs = c("lo", "hi")

SID_mat = array(0, dim = c(length(HIV_labs), length(STI_labs), length(RISK_labs)), dimnames = list(HIV_labs, STI_labs, RISK_labs))



sHIV = list()
sHIV[['S']] = grep("^S.*", HIV_labs, value=TRUE)
sHIV[['I']] = grep("^I.*", HIV_labs, value=TRUE)
sHIV[['D']] = grep("^D.*", HIV_labs, value=TRUE)
sHIV[['I_lo']] = grep("^I_lo.*", HIV_labs, value=TRUE)
sHIV[['I_hi']] = grep("^I_hi.*", HIV_labs, value=TRUE)
sHIV[['D1plus']] = c("D1", "D2", "D3")
sHIV[['D2plus']] = c("D2", "D3")
sHIV[['PLHIV']] = union(sHIV[['I']], sHIV[['D']])

HIV_transitions = rbind(c("S_lo_inf", "S_lo", "I_lo_new"),
                        c("S_hi_inf", "S_hi", "I_hi_new"),
                        c("S_pr_inf","S_pr", "I_pr_new"),
                        c("I_lo_wait_1", "I_lo_new", "I_lo_mid"),
                        c("I_lo_wait_2","I_lo_mid", "I_lo_old"),
                        c("I_hi_wait_1", "I_hi_new", "I_hi_mid"),
                        c("I_hi_wait_2", "I_hi_mid", "I_hi_old"),
                        c("I_pr_wait_1", "I_pr_new", "I_pr_mid"),
                        c("I_pr_wait_2", "I_pr_mid", "I_pr_old"),
                        c("I_lo_new_d", "I_lo_new", "D1"),
                        c("I_lo_mid_d", "I_lo_mid", "D1"),
                        c("I_lo_old_d", "I_lo_old", "D1"),
                        c("I_hi_new_d", "I_hi_new", "D1"),
                        c("I_hi_mid_d", "I_hi_mid", "D1"),
                        c("I_hi_old_d", "I_hi_old", "D1"),
                        c("I_pr_new_d", "I_pr_new", "D1"),
                        c("I_pr_mid_d", "I_pr_mid", "D1"),
                        c("I_pr_old_d", "I_pr_old", "D1"),
                        c("treat", "D1", "D2"),
                        c("viral_supp", "D2", "D3")
                        )
colnames(HIV_transitions) = c("trans", "from", "to")

tHIV = list()
tHIV[['inf']] = grep("inf", HIV_transitions, value=TRUE)
tHIV[['wait_1']] = grep("wait_1", HIV_transitions, value=TRUE)
tHIV[['wait_2']] = grep("wait_2", HIV_transitions, value=TRUE)
tHIV[['wait']] = grep("wait", HIV_transitions, value=TRUE)
tHIV[['test_new']] = grep("I_.._new_d", HIV_transitions, value=TRUE)
tHIV[['test_mid']] = grep("I_.._mid_d", HIV_transitions, value=TRUE)
tHIV[['test_old']] = grep("I_.._old_d", HIV_transitions, value=TRUE)
tHIV[['test']] = grep("I_.._..._d", HIV_transitions, value=TRUE)

