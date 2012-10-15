void exec (char* start) {
  void* jumptable[15]={&&halt,&&val,&&lref,&&lset,
                       &&gref,&&gset,&&def,&&lambda,
                       &&prim,&&jump,&&fjump,&&tjump,
                       &&save,&&apply,&&ret};
  goto *(jumptable[*start]);
  do {
  halt: 
    break;
  val: 
    bard_value val = decode_value(start+1);
    vm_pushval(val);
    start+=5;
    goto *(jumptable[*start]);
    break;
  lref: 
    char i = decode_byte(start+1);
    char j = decode_byte(start+2);
    bard_value val = env_ref(i,j);
    vm_pushval(val);
    start+=3;
    goto *(jumptable[*start]);
    break;
  lset: 
    char i = decode_byte(start+1);
    char j = decode_byte(start+2);
    bard_value val = decode_value(start+3);
    env_set(i,j,val);
    start+=6;
    goto *(jumptable[*start]);
    break;
  gref: 
    bard_value nm = decode_value(start+1);
    bard_value val = global_ref(nm);
    vm_pushval(val);
    start+=5;
    goto *(jumptable[*start]);
    break;
  gset: 
    bard_value nm = decode_value(start+1);
    bard_value val = decode_value(start+5);
    vm_pushval(val);
    global_set(nm,val);
    start+=9;
    goto *(jumptable[*start]);
    break;
  def: 
    bard_value nm = decode_value(start+1);
    bard_value val = decode_value(start+5);
    vm_pushval(val);
    global_def(nm,val);
    start+=9;
    goto *(jumptable[*start]);
    break;
  lambda: 
    bard_value val = decode_value(start+1);
    bard_value fn = make_method(val);
    vm_pushval(fn);
    start+=5;
    goto *(jumptable[*start]);
    break;
  prim: 
    bard_value pr = decode_value(start+1);
    bard_value arglist = decode_value(start+5);
    bard_value val = apply_prim(pr,arglist);
    vm_pushval(val);
    start+=9;
    goto *(jumptable[*start]);
    break;
  jump: 
    bard_value val = decode_value(start+1);
    long dst = as_long(val);
    start=dst;
    goto *(jumptable[*start]);
    break;
  fjump: 
    bard_value testval = vm_popval();
    bard_value val = decode_value(start+1);
    long dst = as_long(val);
    bool test_is_false = is_false(testval);
    if (test_is_false) {
      start=dst;
    } else {
      start+=1;
    }
    goto *(jumptable[*start]);
    break;
  tjump: 
    bard_value testval = vm_popval();
    bard_value val = decode_value(start+1);
    long dst = as_long(val);
    bool test_is_true = is_true(testval);
    if (test_is_true) {
      start=dst;
    } else {
      start+=1;
    }
    goto *(jumptable[*start]);
    break;
  save: 
    bard_value val = decode_value(start+1);
    long dst = as_long(val);
    vm_pushstate(dst);
    start+=1;
    goto *(jumptable[*start]);
    break;
  apply: 
    bard_value val = decode_value(start+1);
    long nargs = as_long(val);
    vm_apply(nargs);
    start+=5;
    goto *(jumptable[*start]);
    break;
  ret:    
    vm_restore();
    start+=1;
    goto *(jumptable[*start]);
    break;

  } while(1);

}
