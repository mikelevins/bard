# Bard 0.4 VM

## Instructions


| OP | args | Notes |
| -- | ---- | ----- |
| HALT |  | Halt the VM |
| CONST | _r_ _k_ | Load constant _k_ in register _r_ |
| MOVE | rdest rsrc |  |
| LREF | r i j |  |
| LSET | r i j |  |
| GREF | r g |  |
| GSET | r g |  |
| JUMP | d |  |
| FJUMP | d |  |
| TJUMP | d |  |
| SAVE | d |  |
| CALL | nargs |  |
| RETURN |  |  |
| ARGS | r n |  |
| ARGS. | r n |  |
| FN | rdest rargs rbody |  |
| PRIM | r p |  |
| SETCC | d |  |
| CC | r |  |
| CONS | rdest rcar rcdr |  |
| CAR | rdest rsrc |  |
| CDR | rdest rsrc |  |
| NOT | rdest rsrc |  |
| LIST1 | rdest rsrc |  |
| LIST2 | rdest rarg1 rarg2 |  |
| LIST3 | rdest rarg1 rarg2 rarg3 |  |
| COMP | rdest rsrc |  |
| OPENFILE | rdest rsrc |  |
| CLOSEFILE | rdest rsrc |  |
| OPENSCK | rdest rsrc |  |
| CLOSESCK | rdest rsrc |  |
| OPENBUF | rdest rsrc |  |
| CLOSEBUF | rdest rsrc |  |
| OPENWIN | rdest rsrc |  |
| CLOSEWIN | rdest rsrc |  |
| RDBYTES | rdest rsrc |  |
| WRBYTES | rdest rsrc |  |
| RDIMG | rsrc | Read a heap image from port _rsrc_, replacing the current VM state |
| WRIMG | rdest | Write the current VM state to port _rdest_ |
| READ | rdest rsrc |  |
| WRITE | rdest rsrc |  |
| DISP | rdest rsrc |  |
| RANDOM | rdest rsrc |  |
| ADD | rdest rarg1 rarg2 |  |
| SUB | rdest rarg1 rarg2 |  |
| MUL | rdest rarg1 rarg2 |  |
| DIV | rdest rarg1 rarg2 |  |
| LT | rdest rarg1 rarg2 |  |
| GT | rdest rarg1 rarg2 |  |
| LTE | rdest rarg1 rarg2 |  |
| GTE | rdest rarg1 rarg2 |  |
| NEQ | rdest rarg1 rarg2 |  |
| EQ | rdest rarg1 rarg2 |  |
| EQL | rdest rarg1 rarg2 |  |
| EQUAL | rdest rarg1 rarg2 |  |
| NIL |  |  |
| TRUE |  |  |
| FALSE |  |  |
| NEG1 |  |  |
| ZERO |  |  |
| ONE |  |  |
| TWO |  |  |
