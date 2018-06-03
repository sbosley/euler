require 'mathn.rb'
def largest_prime_factor(n)
	p = Prime.new
	upper = Math.sqrt(n)
	largest = 1
	while n > 1
		nextP = p.next
		break if nextP > upper
		if n % nextP == 0
			puts "FOUND FACTOR #{nextP}"
			largest = nextP
			while n % nextP == 0
				n = n / nextP
			end
		end
	end
	largest
end
	
