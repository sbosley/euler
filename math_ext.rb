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

	def self.sum_primes(n)
		list = (2..n).to_a
		sum = 0
		count = 0

		while !list.empty?
			sum += (prime = list.shift)
			puts "SO FAR #{sum}, LAST PRIME #{prime}" if count % 500 == 0
			count += 1
			list = list.reject {|item| item % prime == 0}
		end
		sum

	end

	def self.num_primes(n)
		self.primes(n).count
	end
end

def sum_chars(i)
	str = i.to_s
	sum = 0
	str.each_char {|c| sum += c.to_i}
	sum
end

def fact(n)
	(1..n).inject(:*)
end

class Fibbonacci

	def initialize
		@cache = [1, 1]
	end

	def populate_up_to_n(n)
		while @cache.length < n
			@cache << (@cache[@cache.length - 2] + @cache[@cache.length - 1])
		end
	end
	
	def nth(n)	
		self.populate_up_to_n(n)
		@cache[n - 1]
	end

	def first_n(n)
		self.populate_up_to_n(n)
		@cache.slice(0, n)
	end
end








