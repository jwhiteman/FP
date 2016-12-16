Y = fun(C) ->
        (fun(H) ->
             H(H)
         end)(
           fun(F) ->
               C(fun(X) -> (F(F))(X) end)
           end
          )
    end.

Z = fun(M) ->
        fun(0) -> 1;
           (N) -> N * M(N-1) end
    end.

(Y(Z))(10).
