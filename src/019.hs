import           Data.Time.Calendar          (fromGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
main=undefined
months = [fromGregorian y m d|y<-[1901..2000],m<-[1..12],d<-[1]]
sundays = filter sunday months where
  sunday d = trd (toWeekDate d) == 7
  trd (_,_,x) = x

answer = length sundays

coins = [200,100,50,20,10,5,2,1]
ns = [1,0,0,0,0,0,0,0]
ts = [1,2,4,10,20,40,100,200]

-- p xs | a==0 = [xs]
-- where
--   a = find
--   downs = xs
