u_corrector <- function(U_ppm, age_myr,
                                                 lambda_238 = 1.55125e-10,  # Decay constant for U-238
                                                 lambda_235 = 9.8485e-10,   # Decay constant for U-235
                                                 u238_u235_ratio = 137) {   # Natural U-238 to U-235 ratio

  # Convert age from Myr to years
  age_years <- age_myr * 1e6

  # Partition the total U concentration based on the natural ratio
  U238_ppm <- U_ppm * (u238_u235_ratio / (u238_u235_ratio + 1))  # U-238 part
  U235_ppm <- U_ppm / (u238_u235_ratio + 1)                      # U-235 part

  # Calculate original U amount for U-238 and U-235 in the past
  U238_original <- U238_ppm / exp(-lambda_238 * age_years)
  U235_original <- U235_ppm / exp(-lambda_235 * age_years)

  # Sum the original amounts of U-238 and U-235 to get the total original U
  U_original <- U238_original + U235_original

  return(U_original)
}
