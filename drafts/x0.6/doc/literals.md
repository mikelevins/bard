## Literals

* Base singletons

    undefined
    nothing  
    true     
    false    
    end      

* Atoms

    <character>     #\a #\B #\space
    <fixnum>        0 1 999
    <bignum>        9999999999999999999999999999999999999
    <single-float>  1.0
    <double-float>  1.0d0
    <complex>       2+3i
    <ratio>         2/3
    <keyword>       :Foo
    <symbol>        Bar

* Collections

    List  (a b c) ; unless quoted, this expression will be treated as a function call
          [a b c]

    Map   {:a 1 :b 2}    

**Type-constraint** expressions

    bard> (type-of #[<cons>][1 2 3])
    <cons>

    bard> (type-of #[<wb-sequence>][1 2 3])
    <wb-sequence>

    bard> (type-of #[<wb-map>]{1 2 3 4})
    <wb-map>

    bard> (type-of #[<cons>]{1 2 3 4})
    ERROR: can't read expression "{1 2 3 4}" as type <cons>

Type constraints work with numbers as well:

    #[<bignum>] 999

    #[<single-float>] 999




