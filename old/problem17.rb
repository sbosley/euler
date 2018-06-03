class NumberCharCounter
	TENS = {2 => "twenty".length, 3 => "thirty".length, 4=> "forty".length, 5 => "fifty".length, 6 => "sixty".length, 7 => "seventy".length, 8 => "eighty".length, 9 => "ninety".length }

	TEENS = {10 => "ten".length, 11 => "eleven".length, 12 => "twelve".length, 13 => "thirteen".length, 14 => "fourteen".length, 15 => "fifteen".length, 16 => "sixteen".length, 17 => "seventeen".length, 18 => "eighteen".length, 19 => "nineteen".length}

	ONES = {0 => 0, 1 => "one".length, 2 => "two".length, 3 => "three".length, 4 => "four".length, 5 => "five".length, 6 => "six".length, 7 => "seven".length, 8 => "eight".length, 9 => "nine".length }

	
	def initialize
		@cache = {}
	end

	def chars_in_num(n)
		return @cache[n] if @cache.has_key?(n)
		return "onethousand".length if (n == 1000)	
		original = n
		count = 0
		hundreds = n / 100
		used_hundreds = false
		if (hundreds > 0)
			count += ONES[hundreds] + "hundred".length
		end
		n = n % 100
		
		tens = n / 10
		if (tens == 1)
			count += TEENS[n]
			count += "and".length if hundreds > 0
			@cache[original] = count
			return count
		elsif (tens > 1)
			count += TENS[tens]
		end
	
		ones = n = n % 10
		count += ONES[n]
		count += "and".length if (hundreds > 0 && (tens > 0 || ones > 0))
		@cache[original] = count
		count
	end

	def self.count_up_to(n)
		counter = self.new
		(1..n).reduce(0) {|acc, item| acc + counter.chars_in_num(item)}
	end

end
