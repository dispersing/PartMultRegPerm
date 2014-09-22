#################################################
#                                               #
#           FREEMAN AND LANE (1983)             #
#       via Anerson and Legendre (2001)         #
#  PERMUTATION METHOD FOR MULTIPLE REGRESSSION  #
#                                               #
#                                               #
#################################################

# 0. create a full model with one predictor significant (X), the other not (Z)
# 1. run a full model analysis
# 2. run reduced model
# 3. permute reduced model residuals
# 4. run regression of permuted resibuals on reduced model
# 5. take estimated Y (E(Y)) and put into the full model
# 6. repeate 3.-5. M times
# 7. t-reference from 1. is then compared to the distribution of t-star, from 5.

# 0. create a full model with one predictor significant (X), the other not (Z)

	rm(list = ls())
	set.seed(6174) #Kaprekar's constant
	n <- 50
	X <- runif(n , 10 , 20)
	Z <- runif(n , 10 , 20)
	Y <- X + rnorm(n , 0  , 1)
		par(mfrow=c(1,2) , pin = rep(2.5,2))
		plot(Y ~ X)
		abline(lm(Y ~ X))
		plot(Y ~ Z)
		abline(lm(Y ~ Z))


runs <- 10
t.star <- c()
for (i in 1:runs)
{
# 1. run a full model analysis
	full.mod <- lm(Y ~ X + Z)
		# summary(full.mod)

# 2. run reduced model
	sig.vars <- c()
	t.ref <- c()
	coefs <- c()
	num.betas <- length(summary(full.mod)$coefficients[,2])
	not.sig <- c()

	for (p in 1:num.betas){
			t.ref[p] <- summary(full.mod)$coefficients[p,3]
			sig.vars[p] <- dimnames(summary(full.mod)$coefficients)[[1]][p]
			coefs[p] <- summary(full.mod)$coefficients[p,1]
			if (summary(full.mod)$coefficients[p,4] > 0.05) {not.sig[p] <- p}
	}

		sig <- match(NA , not.sig)
		not.sig <- not.sig[!is.na(not.sig)]
		t.ref <- t.ref[-not.sig]
		sig.vars <- sig.vars[-not.sig]
		sig.vars <- paste(sig.vars, sep ="+")
		coefs <- coefs[-not.sig[-1]]

# 3. permute reduced model residuals
	red.mod <- lm(paste("Y ~",sig.vars,sep = ""))
	red.mod.res <- summary(red.mod)$residuals
	perm.mod.res <- sample(red.mod.res)

# 4. run regression of permuted resibuals on reduced model
	Y.star <- coefs[1] + coefs[-1]*X + perm.mod.res

# 5. take estimated Y (E(Y)) and put into the full model
	EY <- lm(Y.star ~ X + Z)
	t.star[i] <- summary(EY)$coefficients[sig,3]
}