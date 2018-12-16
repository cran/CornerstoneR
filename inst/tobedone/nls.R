# nlr.R: partial linear / non-linear regression
# transform the input data
trans.rsp = switch( trn,
	invsquare = paste( "(" , rsp , ")^-2" ),
	inv = paste( "1/(" , rsp , ")" ),
	invroot= paste( "1/sqrt(" , rsp , ")" ),
	log = paste( "log(" , rsp , ")" ), 
	root= paste( "sqrt(" , rsp , ")" ),
	square= paste( "(" , rsp , ")^2" ),
	rsp
)
data.transformed <- transform( cs.in.dataset(), response = eval( parse( text = trans.rsp )))
data.included <- subset(data.transformed, !cs.in.excluded())

# perform partial linear / non-linear regression
nls1 <- nls( 
	formula = formula( paste( "response ~ cbind(model.matrix(~",lin,")," , nln , ")" ) ), 
	start = eval( parse( text = paste( "list(" , start , ")" ))),	
	data = data.included,
	algorithm="plinear",
	trace = trc,
	weights = data.included[[weights]],
	control = c( maxiter=maxiter, tol=tol )
)

# coefficient dataset (Coefficient, Value)
cf <- data.frame( coef( nls1 ) )
coefficients <- data.frame(Coefficient=row.names(cf), Value=cf[[1]])
cs.out.dataset(coefficients)

# fit estimate (predicted, response, residual)
# computed for all input rows (even the excluded ones!) and brushable
predicted <- predict(nls1, newdata=data.transformed)
fit.estimate <- data.frame(predicted, data.transformed["response"])
fit.estimate <- transform(fit.estimate, residual = response - predicted)
cs.out.dataset(fit.estimate, name = "Fit Estimate", brush = TRUE)





