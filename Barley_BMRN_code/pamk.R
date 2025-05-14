library('fpc')
pamk_best <- pamk(data_clean_log2_zs)$nc 
plot(pam(data_clean_log2_zs,pamk_best))