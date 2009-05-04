# start here...
fac = ->(f) {
        ->(n) {
          if n.zero?
            1
          else
            n * f.(f).(n-1)
          end
         }
       }
fac.(fac).(10)


# not too bad to get here...
->(h) {
  h.(h)
}.(->(f) {
     ->(n) {
       if n.zero?
         1
       else
         n * f.(f).(n-1)
       end
     }
  })
fac.(10)


# ok...had to look that trick up. kinda understand.
->(h) {
  h.(h)
}.(->(f) {
     ->(n) {
       if n.zero?
         1
       else
         n * ->(x) { f.(f).(x) }.(n-1)
       end
     }
  })


# this part is a little simpler...
->(h) {
  h.(h)
}.(->(f) {
     ->(m) {
       ->(n) {
          if n.zero?
            1
          else
            n * m.(n-1)
          end
        }
     }.(->(x) { f.(f).(x) })
  })


# another easy step, but starting to look crazy...
->(le) {
  ->(h) {
    h.(h)
   }.(->(f) {
        le.(->(x) { f.(f).(x) })
     })
}.(->(m) {
    ->(n) {
      if n.zero?
        1
      else
        n * m.(n-1)
      end
    }
 })

# Huzzah for Y!
Y = ->(le) {
      ->(h) {
        h.(h)
      }.(->(f) {
           le.( ->(x) { f.(f).(x) } )
         })
      }
      
factorial = ->(f) {
  ->(n) {
    if n.zero?
      1
    else
      n * f.(n-1)
    end
  }
}

(1..10).map(&Y.(factorial))


(1..10).map(&->(le) {
      ->(h) {
        h.(h)
      }.(->(f) {
           le.( ->(x) { f.(f).(x) } )
         })
      }.(->(f) {
          ->(n) {
            if n.zero?
              1
            else
              n * f.(n-1)
            end
           }
         }))

      

# CRAZY ASS FP REFACTORING PART I
# pattern of turning f.(p) into m.()
f.(p).(42)

->(x) { f.(p).(x) }.(42)

->(m) {
  m.(42)
}.(->(x) { f.(p).(x) })

