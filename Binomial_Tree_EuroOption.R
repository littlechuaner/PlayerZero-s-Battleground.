source("binomial.R")
tree=build_stock_tree_simple(100,1.1,0.9,0.2,5)
q_prob_simple(0.04,0.2,1.1,0.9)
value_binomial_option_simple(tree,1.1,0.9,0.2,0.02,100,"call")
#binomial_option_simple = function(type,T,r,X,S,N,u,d)
binomial_option_simple("call",2,0.03,100,100,3,1.1,0.9)
#####
binomial_option_simple("put",3,0.03,100,100,10,1.1,0.9,4,5)
#######
Portfolio_building("call",2,0.03,100,100,3,1.1,0.9,2,1)
source("BSM_option.R")
BSM(100,80,0.04,0.05,3,0,0,"call")