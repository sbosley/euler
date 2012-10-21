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
