def is_palindrome(str)
	while str.length > 1
		first = str[0]
		last = str[str.length - 1]
		return false if (first != last)
		str = str[1, str.length - 2]
	end
	true
end

def largest_palindrome
	largest = 0
	for a in (100..999)
		for b in (100..999)
			product = a * b
			largest = product if (product > largest && is_palindrome(product.to_s))
		end
	end
	largest
end
