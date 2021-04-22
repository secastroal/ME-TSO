metso.var.coeff <- function(within.parameters, between.parameters, ind.ar, id){
  
  bp <- between.parameters
  wp <- within.parameters
  
  # Coefficients across fixed situations ----
  # Fixed situation variances
  fxi.var <- bp$xi.var + 2 * bp$xi.var * bp$beta1 + bp$xi.var * (bp$beta1 ^ 2) + bp$omega.var 
  # Reference X fixed situations covariances
  fxi.cov <- bp$xi.var + bp$xi.var * bp$beta1
  # Reference X fixed situations correlations
  fxi.cor <- fxi.cov / sqrt(bp$xi.var * fxi.var)
  
  # Consistency of traits
  con.trait <- data.frame(round((fxi.cor) ^ 2, 2))
  # Situation specificity of traits
  spe.trait <- data.frame(round(1 - con.trait, 2))
  
  names(con.trait) <- names(spe.trait) <- paste0("Xi_j", 1:(dim(con.trait)[2]))
  
  # Total variance of Gamma
  gamma.var <- bp$xi.var * (bp$beta1 ^ 2) + bp$omega.var
  # Person-situation interaction coefficient
  int.coeff  <- data.frame(round((bp$xi.var * (bp$beta1 ^ 2)) / gamma.var, 2))
  unique.eff <- data.frame(round(bp$omega.var / gamma.var, 2))
  
  names(int.coeff) <- names(unique.eff) <- names(con.trait)
  
  coeff.fixed <- list(trait.consistency = con.trait,
                      trait.specificity = spe.trait,
                      interaction.coeff = int.coeff,
                      unique.effect     = unique.eff)
  
  # Coefficients across random situations within fixed situations----
  #create list to store all the coefficients across random situations
  coeff.random <- list()
  
  # Reference situation
  # Total variance
  trait.var       <- matrix(bp$xi.var, length(id), length(bp$xi.var), byrow = TRUE)
  state.loadings  <- matrix(wp$lambda.state, length(id), length(bp$xi.var), byrow = TRUE)
  state.resid     <- matrix(wp$epsilon.var, length(id), length(bp$xi.var), byrow = TRUE)
  autoreg.effects <- as.vector(ind.ar ^ 2 / (1 - ind.ar ^ 2))
  
  tot.var <- trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2 +
    wp$zeta.var * state.loadings ^ 2 + state.resid
  
  rel   <- data.frame(round((trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2 +
            wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # reliability
  con   <- data.frame(round((trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # consistency
  pred  <- data.frame(round((trait.var) / tot.var, 2)) # predictability by trait
  upred <- data.frame(round((autoreg.effects * wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # unpredictability by trait
  spe   <- data.frame(round((wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # occasion specificity
  
  names(rel) <- names(con) <- names(pred) <-
    names(upred) <- names(spe) <- paste0("Y_", 1:length(bp$xi.var), "0")
  row.names(rel) <- row.names(con) <- row.names(pred) <-
    row.names(upred) <- row.names(spe) <- id
  
  coeff.random[[1]] <- list(reliability = rel,
                            consistency = con,
                            specificity = spe,
                            predictability   = pred,
                            unpredictability = upred)
  
  for (f in 1:(dim(fxi.var)[2])) {
    trait.var       <- matrix(fxi.var[, f], length(id), length(bp$xi.var), byrow = TRUE)
    state.loadings  <- matrix(wp$lambda.state, length(id), length(bp$xi.var), byrow = TRUE)
    state.resid     <- matrix(wp$epsilon.var, length(id), length(bp$xi.var), byrow = TRUE)
    autoreg.effects <- as.vector(ind.ar ^ 2 / (1 - ind.ar ^ 2))
    
    tot.var <- trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2 +
      wp$zeta.var * state.loadings ^ 2 + state.resid
    
    rel   <- data.frame(round((trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2 +
                      wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # reliability
    con   <- data.frame(round((trait.var + autoreg.effects * wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # consistency
    pred  <- data.frame(round((trait.var) / tot.var, 2)) # predictability by trait
    upred <- data.frame(round((autoreg.effects * wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # unpredictability by trait
    spe   <- data.frame(round((wp$zeta.var * state.loadings ^ 2) / tot.var, 2)) # occasion specificity
    
    names(rel) <- names(con) <- names(pred) <-
      names(upred) <- names(spe) <- paste0("Y_", 1:length(bp$xi.var), f)
    row.names(rel) <- row.names(con) <- row.names(pred) <-
      row.names(upred) <- row.names(spe) <- id
    
    coeff.random[[f + 1]] <- list(reliability = rel,
                                  consistency = con,
                                  specificity = spe,
                                  predictability   = pred,
                                  unpredictability = upred)
  }
  
  names(coeff.random) <- paste0("Xi_j", 0:(dim(fxi.var)[2]))
  
  out <- list(fixed.situations  = coeff.fixed,
              random.situations = coeff.random)
  
  return(out)
}
