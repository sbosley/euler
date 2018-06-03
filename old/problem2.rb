MAX_FIB = 4000000

def fibb_sum
	f0 = 1
	f1 = 1
	sum = 0
	while f1 < MAX_FIB do
		sum += f1 if (f1 % 2 == 0)
		new = f0 + f1
		f0 = f1
		f1 = new
	end
	sum
end
