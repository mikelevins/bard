// bardvm

// vm state registers

var bardvm =  {
    pc: 0,
    code: [],
    stack: [],
    env: [],
    globals: {},
    isHalted: false
};

// instruction opcodes

var HALT=0;
var LREF=1;
var LSET=2;
var GREF=3;
var GSET=4;
var CONST=5;
var GO=6;
var TGO=7;
var FGO=8;
var SAVE=9;
var CALL=10;
var RETURN=11;
var ARGS=12;
var RESTARGS=13;
var CC=14;
var SETCC=15;
