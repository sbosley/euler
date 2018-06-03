def natural_sum
	(1...1000).reduce(0)  {|sum, n| 
		if (n % 3 == 0 || n % 5 == 0) then
			sum + n
		else
			sum
		end
	}
end
		
