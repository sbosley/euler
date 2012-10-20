class SieveOfEra
	def self.primes(n)
		list = (2..n).to_a
		primes = []
		while !list.empty?
			primes << prime = list.shift
			list = list.reject {|item| item % prime == 0}
		end
		primes
	end

	def self.num_primes(n)
		self.primes(n).count
	end
end
