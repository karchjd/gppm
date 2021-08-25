
	data{
		int<lower=1> n;
		vector[n] x;
		}	
	parameters{
		real mu;
		real<lower=0> sigma;
		}	
	model{
		x ~ normal(mu, sigma);
		}
	