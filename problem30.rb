require './math_ext.rb'
def find_fifth_power_sums
	max = 99999
	sum = 0
	(2..max).each {|n|
		digits = digits_list(n)
		fifth_power_sum = digits.reduce(0) {|acc, d| acc += d**5}
		sum += n if fifth_power_sum == n
	}
	sum
end
