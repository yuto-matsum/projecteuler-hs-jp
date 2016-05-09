main=undefined
p=2^1000
a n= take n [div (mod p x) (div x 10) | i<-[1..], let x=10^i]
answer n= sum $ a n
