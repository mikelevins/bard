# bard 0.4

## Lexical syntax

#### numbers
    0
    1
    2/3
    1.2
    1e6
    1e-6
    #b1011
    #xFACE

#### names

##### named constants

    nothing
    anything
    true
    false
    end

##### symbols

    red
    blue
    Fred
    Barney
    *standard-output*

##### keywords

    :foo
    :Bar


##### URIs

    #<uri> "http://bardcode.net"

#### text strings

    "A sample text string"

#### lists

    '()
    '(1 2 3)
    '(:red :blue :green)
    []
    [1 2 3]
    [:red :blue :green]

#### maps

    {}
    {:left 1 :right 2}

#### methods

    (^ (x) x)
    (^ (x y) (+ x y))

#### functions

    (-> ->)
    (-> Number ->)
    (-> Number -> Number)
    (-> Number Number -> Boolean)
    (-> List Number -> anything)
    (-> Function Function List -> Number Boolean)
    (-> ... -> ...) ; any number of inputs, any number of outputs
    (->  -> Anything ...) ; no inputs, one or more outputs

#### reader type constraints

    #<small-integer> 12
    #<big-integer> 12
    #<cons> [1 2 3]
    #<vector> [1 2 3]

## Special forms

**`^`**

**`->`**

**`begin`**

**`bound?`**

**`cond`**

**`def`**

**`define macro`**

**`define method`**

**`define protocol`**

**`define record`**

**`define vector`**

**`ensure`**

**`generate`**

**`if`**

**`let`**

**`loop`**

**`match`**

**`not`**

**`quasiquote`**

**`quote`**

**`receive`**

**`repeat`**

**`send`**

**`set!`**

**`time`**

**`unbind!`**

**`undefine`**

**`unless`**

**`values`**

**`when`**

**`with-exit`**

**`with-open`**

## Protocols

### **`Conditions`**
### **`Construction`**
### **`Control`**
### **`Conversion`**
### **`Copying`**
### **`Equality`**
### **`Functions`**
### **`Iteration`**
### **`Lists`**
### **`Macros`**
### **`Maps`**
### **`Messages`**
### **`Names`**
### **`Ordering`**
### **`Packages`**
### **`Pairs`**
### **`Streams`**
### **`System`**
### **`Text`**
### **`Types`**

