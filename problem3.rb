require './primes.rb'
def largest_prime_factor(n)
	primes = SieveOfEra.primes(Math.sqrt(n).floor)
	primes.reverse_each {|p|
		return p if n % p == 0
	}
	1
end
	
