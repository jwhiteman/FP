def fibonacci(n)
  if n < 2
    1
  else
    fibonacci(n-1) + fibonacci(n-2)
  end
end


$cache = Hash.new

def fibonacci(n)
  if n < 2
    1
  else
    ($cache[n-1] ||= fibonacci(n-1)) +
      ($cache[n-2] ||= fibonacci(n-2))
  end
end

$fibonacci = Hash.new do |h,k|
  h[k] = (k < 2) ? 1 : h[k-1] + h[k-2]
end


# this works differently than i expected
def factorial(n, remaining_work)
  if n.zero?
    remaining_work.call 1
  else
    factorial n-1, ->(x) { remaining_work.call(x * n) }
  end
end

# 1.8.6
# think of those nested russian dolls -- matryoshka dolls
def factorial(n, cc)
   if n.zero?
     proc { cc.call(1) }
   else
     factorial n-1, proc { |x| cc.call(n * x) }
   end
 end



factorial  5, ->(n) { puts n }
factorial  4, ->(x) { ->(n) { puts n }.call(x * 5) }
factorial  3, ->(x) { ->(x) { ->(n) { puts n }.call(x * 5) }.call(x * 4)  }
factorial  2, ->(x) { ->(x) { ->(x) { ->(n) { puts n }.call(x * 5) }.call(x * 4)  }.call(x * 3) }
factorial  1, ->(x) { ->(x) { ->(x) { ->(x) { ->(n) { puts n }.call(x * 5) }.call(x * 4)  }.call(x * 3) }.call(x * 2) }
factorial  0, ->(x) { ->(x) { ->(x) { ->(x) { ->(n) { puts n }.call(x * 5) }.call(x * 4)  }.call(x * 3) }.call(x * 2) }.call(1)


->(x) { 
  ->(x) { 
    ->(x) { 
      ->(x) { 
        ->(n) { 
          puts n 
        }.call(x * 5) 
      }.call(x * 4)  
    }.call(x * 3) 
  }.call(x * 2) 
}.call(1) # termination point