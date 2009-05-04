# stage: ERROR!
fac = ->(n) {
  if n < 2
    1
  else
    n * fac(n-1)
  end
}

# stage: fine, then. tell me what to do (but we're still not sure yet)
fac = ->(f) {
  ->(n) {
    if n < 2
      1
    else
      n * f.(n-1)
    end
  }
} 

fac.( ->(x) { raise "DIE! DIE! DIE!" } ).(1)
fac.( ->(x) { raise "DIE! DIE! DIE!" } ).(2) rescue "dead."

fac.(fac).(2) # => 2 * proc == ERRORZ. DAG, YO!


# stage: leap of faith
fac = ->(f) {
  ->(n) {
    if n < 2
      1
    else
      n * f.(f).(n-1)
    end
  }
}

fac.(fac).(500) # WOOT!


# stage: fac.(fac) is lamerz
fac = ->(f) { 
          f.(f) 
        }.(->(f) {
          ->(n) {
            if n < 2
              1
            else
              n * f.(f).(n-1)
            end
          }
        })
        
        
# stage: who really knows who came up with this shit
fac => ->(f) {
          f.(f)
        }.(
          
        )
        
        
        
###
->(le) {
  ->(h) {
    h.(h)
  }.(->(f) {
    le.(->(x) { f.(f).(x) })
  })
}.(
->(m) {
   ->(n) {
      if n.zero?
        1
      else
        n * m.(n-1)
      end
    }
 })  