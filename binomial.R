q_prob = function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp(r*delta_t) - d)/(u-d))
}
####
q_prob_simple = function(r, delta_t,u,d) {
  return((exp(r*delta_t) - d)/(u-d))
}
build_stock_tree = function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}
########
build_stock_tree_simple = function(S,u,d,delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)

  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}
########
value_binomial_option = function(tree, sigma, delta_t, r, X, type) {
  q = q_prob(r, delta_t, sigma)

  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }

  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}
####
value_binomial_option_simple = function(tree,u,d, delta_t, r, X, type) {
  q = q_prob_simple(r, delta_t,u,d)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}
binomial_option = function(type, sigma, T, r, X, S, N) {
  q = q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree = build_stock_tree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option = value_binomial_option(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  delta = (option[2,2]-option[2,1])/(tree[2,2]-tree[2,1])
  return(list(q=q, stock=tree, option=option, price=option[1,1], delta=delta))
}
#####
binomial_option_simple = function(type,T,r,X,S,N,u,d,nodX,nodY) {
  q = q_prob_simple(r=r, delta_t=T/N, u=u,d=d)
  tree = build_stock_tree_simple(S=S,u=u,d=d, delta_t=T/N, N=N)
  option = value_binomial_option_simple(tree, u=u,d=d, delta_t=T/N, r=r, X=X, type=type)
  b = (option[nodX+1,nodY+1]-option[nodX+1,nodY])/(tree[nodX+1,nodY+1]-tree[nodX+1,nodY])
  a = exp(T/N*r)*(option[nodX+1,nodY+1]-b*tree[nodX+1,nodY+1])
  return(list(q=q, stock=tree, option=option, price=option[1,1], delta=b,cash=a))
}

delta_star = function(binomial_option, row, col) {
  stock_tree = binomial_option$stock
  option_tree = binomial_option$option
  return((option_tree[row+1, col+1] - option_tree[row+1, col])/(stock_tree[row+1, col+1] - stock_tree[row+1, col]))
}
##### 
Portfolio_building = function(type,T,r,X,S,N,u,d,nodX,nodY) {
  q = q_prob_simple(r=r, delta_t=T/N, u=u,d=d)
  tree = build_stock_tree_simple(S=S,u=u,d=d, delta_t=T/N, N=N)
  option = value_binomial_option_simple(tree, u=u,d=d, delta_t=T/N, r=r, X=X, type=type)
  b = (option[nodX+1,nodY+1]-option[nodX+1,nodY])/(tree[nodX+1,nodY+1]-tree[nodX+1,nodY])
  a = exp(T/N*r)*(option[nodX+1,nodY+1]-b*tree[nodX+1,nodY+1])
  return(list(cash=a,stock=b))}
