def usage 
  "prog numruns strat1 strat2"
end
num=Integer(ARGV[0])
s1=ARGV[1]
s2=ARGV[2]

raise usage if (ARGV.size != 3 || num < 1 )
1.upto(num) do
  puts `./Game 5 5 #{s1} #{s2} Silent`
end
