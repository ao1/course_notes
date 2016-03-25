library(lpSolveAPI)

# 3 constraints and 9 decision variables
google1 = make.lp(3,9)

#declare constraint matrix

set.column(AirlineConnecting, 1, c(1,1,1,0,0,0,0,0))
set.column(AirlineConnecting, 2, c(1,1,0,1,0,0,0,0))
set.column(AirlineConnecting, 3, c(1,0,0,0,1,0,0,0))
set.constr.type(AirlineConnecting, rep("<=",3))
set.rhs(AirlineConnecting, c(166,166,80))

#declare objective coefficients

set.objfn(AirlineConnecting, c(428,190,642))

# The default setting is minimize for the objective, so we need 
# to tell R that we are maximizing our objective:

lp.control(google1,sense='max')

google1

solve(google1)

get.objective(google1)
get.variables(google1)

