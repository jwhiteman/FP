class Array
  def car
    first
  end
  
  def cdr
    self[1..-1]
  end
  
  def cons(s)
    self.unshift s
  end
end

class Fixnum
  def even?
    (self % 2) == 0
  end
end

def evens_only_star_cc(l, col)
  if l.empty?
    col.call [], 1, 0
  elsif l.car.is_a? Fixnum
    if l.car.even?
      evens_only_star_cc l.cdr, ->(op, ep, os) {
        col.call op.cons(l.car),
                 (ep * l.car),
                 os
      }
    else
      evens_only_star_cc l.cdr, ->(op, ep, os) {
        col.call op,
                 ep,
                 (os + l.car)
      }
    end
  else
    evens_only_star_cc l.car, ->(nop, nep, nos) {
      evens_only_star_cc l.cdr, ->(op, ep, os) {
        col.call op.cons(nop),
                 (ep * nep),
                 (os + nos)
      }
    }
  end
end

evens_only_star_cc((1..10).to_a, ->(output, poe, soo) {
  {:output => output, :product_of_evens => poe, :sum_of_odds => soo   }
})

evens_only_star_cc([1, 2, [3], 4, 5, [6]], ->(output, poe, soo) {
  {:output => output, :product_of_evens => poe, :sum_of_odds => soo   }
})

evens_only_star_cc([1, [2, [3]], 4, [[[5]]], 6, [7, [8]], 9, 10], ->(output, poe, soo) {
  {:output => output, :product_of_evens => poe, :sum_of_odds => soo}
})