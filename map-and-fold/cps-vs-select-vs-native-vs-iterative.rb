require 'benchmark'

$data = Array.new(1000) { rand(1000) }

def select(f, l)
  if l.empty?
    []
  elsif f.(l.first)
    select(f, l[1..-1]).unshift(l.first)
  else
    select(f, l[1..-1])
  end
end

def select_with_cps(f, l, cont)
  if l.empty?
    cont
  elsif f.(l.first)
    select_with_cps f, l[1..-1], ->(acc) { cont.( acc.unshift(l.first) ) }
  else
    select_with_cps f, l[1..-1], cont
  end
end

#select ->(n) { n.odd? }, $data.dup

#(select_with_cps ->(n) { n.odd? }, $data.dup, ->(x) { x }).([])
TIMES = 100_000

def benchmark(msg, &block)
  block.call

  time = Benchmark.realtime { block.call }
  puts "#{msg} RESULT: #{time}"
end

# approx 49 seconds on my laptop
benchmark("select") do
  TIMES.times do
    select ->(n) { n.odd? }, $data.dup
  end
end

cont = select_with_cps ->(n) { n.odd? }, $data.dup, ->(x) { x }

# about 17 seconds on my laptop
benchmark("CPS") do
  TIMES.times do
    cont.([])
  end
end

# about 8 seconds avg
benchmark("native") do
  TIMES.times do
    $data.select(&:odd?)
  end
end

# about 13 seconds average
benchmark("iterative") do
  TIMES.times do
    result = []
    for n in $data
      if n.odd?
        result << n
      end
    end
    result
  end
end