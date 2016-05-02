# bard 0.4

## Lexical syntax

## Named constants
### **`nothing`**
### **`anything`**
### **`true`**
### **`false`**
### **`end`**

## Built-in structures

### Names
#### **`symbol`**
#### **`keyword`**
#### **`uri`**

### Numbers
#### **`small-integer`**
#### **`big-integer`**
#### **`single-float`**
#### **`double-float`**
#### **`s8ratio`**
#### **`u8ratio`**
#### **`s16ratio`**
#### **`u16ratio`**
#### **`s32ratio`**
#### **`u32ratio`**
#### **`s64ratio`**
#### **`u64ratio`**

### Text characters
#### **`character`**

### Sequences
#### **`vector`**
#### **`string`**
#### **`s8vector`**
#### **`u8vector`**
#### **`s16vector`**
#### **`u16vector`**
#### **`s32vector`**
#### **`u32vector`**
#### **`s64vector`**
#### **`u64vector`**

### Maps
#### **`alist-map`**
#### **`hashtable`**
#### **`treemap`**

### Pairs
#### **`cons`**

### Procedures
#### **`method`**
#### **`function`**

### IO streams
#### **`character-input-stream`**
#### **`character-output-stream`**
#### **`octet-input-stream`**
#### **`octet-output-stream`**
#### **`object-input-stream`**
#### **`object-output-stream`**

## Built-in protocols

### **`Conditions`**

### **`Construction`**
#### Functions
##### **`initialize`**

### **`Conversion`**
#### Functions
##### **`as`**

### **`Definition`**
#### Special forms
##### **`define constant`**
`(-> Name Anything -> Anything)`
##### **`define macro`**
`(-> Name Parameters Form ... -> Name)`
##### **`define method`**
`(-> Name Parameters Form ... -> Name)`
##### **`define protocol`**
`(-> Name &key variables special-forms functions -> Name)`
##### **`define record`**
`(-> Name SlotSpec ... -> Name)`
##### **`define variable`**
`(-> Name Anything -> Name)`
##### **`define vector`**
`(-> Name &key count minimum-count maximum-count element-type initial-element initial-contents -> Name)`
#### Functions
##### **`defined?`**
`(-> Name -> Boolean)`
##### **`undefine!`**
`(-> Name ->)`

### **`Equality`**
#### Functions
##### **`equivalent?`**
##### **`equal?`**
##### **`identical?`**

### **`Evaluation`**
#### Special forms
##### **`eval`**
#### Functions
##### **`apply?`**

### **`Functions`**
### **`Lists`**
### **`Macros`**
### **`Maps`**
### **`Math`**
### **`Ordering`**
### **`Packages`**
### **`Pairs`**
### **`Printing`**
### **`Processes`**

### **`ProgramStructure`**
#### Special forms
##### **`begin`**
##### **`case`**
##### **`cond`**
##### **`if`**
##### **`let`**
##### **`loop`**
##### **`match`**
##### **`quasiquote`**
##### **`quote`**
##### **`repeat`**
##### **`unless`**
##### **`when`**
##### **`with-exit`**
#### Functions
##### **`bound?`**

### **`Reading`**
### **`Resources`**
### **`Streams`**
### **`System`**
### **`Text`**

### **`Types`**
