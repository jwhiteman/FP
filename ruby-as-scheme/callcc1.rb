class Array
  def rest
    self[1..-1]
  end
end

# employee.fired? => false
# employee.not.fired? => true
class Object
  def not
    Not.new(self)
  end
  
  class Not
    instance_methods.each { |meth| undef_method(meth) unless meth =~ /\A__/ }
    
    def initialize(subject)
      @subject = subject
    end
    
    def method_missing(meth, *args, &block)
      !@subject.send(meth, *args, &block)
    end
  end
end

def leftmost(list)
  callcc { |c| lm(list, c) }
end

def lm(l, out)
  if l.empty?
    []
  elsif l.first.not.kind_of? Array
    out.call l.first
  else
    lm l.first, out
    lm l.rest, out
  end
end

leftmost %w(1 2 3)
leftmost [[], [[], [[]]], [[[2]]], 3]