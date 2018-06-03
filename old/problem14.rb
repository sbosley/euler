def max_collatz_chain(under_n)
	cache = {}
	max_length = 1
	generator = 1
	(1..under_n).each do |n|
		length = compute_chain_length(n, cache)
		if length > max_length
			generator = n
			max_length = length
		end
	end
	puts "Generator #{generator}"
	max_length
end

def collatz_chain(n)
	chain = []
	while n > 1
		chain << n
		n = next_collatz_num(n)
	end
	chain << 1
end

def next_collatz_num(n)
	if (n % 2 == 0)
		n / 2
	else
		3*n + 1
	end
end

def compute_chain_length(n, cache)
	return 1 if n == 1
	if cache.has_key? n
		cache[n]
	else
		result = 1 + compute_chain_length(next_collatz_num(n), cache)
		cache[n] = result
		result
	end
end
