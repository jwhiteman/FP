class Array
  def rest
    self[1..-1]
  end
end

class Object
  def atom?
    self.kind_of? Symbol
  end
  
  def cons(a, b)
    b.unshift a
  end
  
  def yComb
    lambda do |f|
      f[f]
    end.call(
      lambda do |f|
        yield lambda { |x| f[f][x] }
      end
    )
  end
end


# FAILED TO WORK WITH TRY CATCH
def rember_first_star(a, list)
  r = lambda do |l, c| # don't know the more generic version of yComb (sorry!)
    if l.empty?
      c.call :not_here!
    elsif l.first.atom?
      if l.first == a
        l.rest
      else
        cons l.first, r.call(l.rest, c)
      end
    else
      if callcc { |x| r.call(l.first, x) }.atom?
        cons l.first, r.call(l.rest, c)
      else
        cons r.call(l.first, nil), l.rest
      end
    end
  end
  
  if callcc { |home| r.call(list, home) }.atom?
    list
  else
    r.call(list, nil)
  end

end

rember_first_star :z, [:a, :b, [:c], [:d], :e, :f, [:g]] # pass
rember_first_star :a, [:x, :y, :z, [[], [:b, [:a]]]]     # pass
rember_first_star :a, [[[[[[:y, [[[[[:a, [:b]]]]]]]]]]]] # pass
rember_first_star :a, [[[[[[[[[[:x, [[[[[[[[[[:a]]]]]]]]]]]]]]]]]]]] # fail...too slow