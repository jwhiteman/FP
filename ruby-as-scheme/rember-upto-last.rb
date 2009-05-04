def cons(a, b)
  b.unshift a
end


def rember_upto_last(a, lat)
  result = catch(:jump!) do
    r = lambda do |l| 
      if l.empty?
        []
      elsif l.first == a
        throw :jump!, r.call(l[1..-1])
      else
        cons l.first, r.call(l[1..-1])
      end
    end
    
    r.call(lat)
  end
end


rember_upto_last 'a', %w(x y z)
rember_upto_last 'a', %w(x y z a b c)
rember_upto_last 'a', %w(x y z a b c a p q)

##################  WITH Y-COMBINATOR ###################
def cons(a, b)
  b.unshift a
end

def yComb
  lambda { |f|
    f[f]
  }.call(
    lambda { |f| yield lambda { |x| f[f][x] } }
  )
end

def rember_upto_last(a, lat)
  result = callcc do |c|
    yComb do |f|
      lambda do |l|
        if l.empty?
          []
        elsif l.first == a
          c.call f.call(l[1..-1])
        else
          cons l.first, f.call(l[1..-1])
        end
      end
    end.call(lat)
  end
end

rember_upto_last 'a', %w(x y z)
rember_upto_last 'a', %w(x y z a b c)
rember_upto_last 'a', %w(x y z a b c a p q)