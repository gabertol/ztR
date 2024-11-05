ratio_calculator <- function(dataframe,equation = "watson",...) {

  dataframe %>%
    mutate(ce_nd=ce140_N/nd146_N,
           dy_nd=dy163_N/yb172_N,
           hf_y=hf177/y89,
           yb_gd=y89/gd157_N,
           H_L_ree=(la139_N+ce140_N+pr141_N+nd146_N+pr141_N)/(eu153_N+gd157_N+tb159_N+ho165_N+er166_N+tm169_N+yb172_N+lu175_N),
           sumREE=la139_N+ce140_N+pr141_N+nd146_N+pr147_N+sm147_N+eu153_N+gd157_N+tb159_N+dy163_N+ho165_N+er166_N+tm169_N+yb172_N+lu175_N,
           eu_eu=anomaly(eu153_N,sm147_N,gd157_N),
           ce_ce=anomaly(ce140_N,la139_N,pr141_N),
           crust=crustal_thickness(eu_eu),
           fmq=FMQ(ce140_N,approx_u,ti49,best_age),
           ti_temp=zircon_ti_t(ti_ppm=ti49, equation = equation,...))

}
