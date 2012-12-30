# Bard Toplevel Reference

## constants

`false`

`nothing`

`true`

`undefined`

## Schemas

`<bignum>`

`<boolean>`

`<character>`

`<fixnum>`

`<flonum>`

`<function>`

`<interpreted-method>`

`<iostream>`

`<keyword>`

`<null>`

`<pair>`

`<primitive-method>`

`<primitive-procedure>`

`<ratnum>`

`<record>`

`<string>`

`<symbol>`

`<table>`

`<undefined>`

## Classes

`Anything``

`Boolean`

`Character`

`Float`

`Fraction`

`Function`

`Integer`

`Keyword`

`List`

`Method`

`Name`

`Pair`

`Null`

`Number`

`Ratio`

`Schema`

`Symbol`

`Table`

`Text`

`Undefined`

## Methods, Functions, Macros, Primitives, and Special Forms

`(as Type Anything) -> Type`

`(boolean? Anything) -> Boolean`

`(by Integer List) -> List`

`(character? Anything) -> Boolean`

`(complement Applicable) -> Applicable`

`(constantly Anything) -> Method`

`(contains-key? Table) -> Boolean`

`(contains-value? Table) -> Boolean`

`(current-input) -> InputStream`

`(current-output) -> OutputStream`

`(display Anything)`

`(drop Integer List) -> List`

`(element List Integer) -> `

`(empty? List) -> Boolean`

`(error Text)`

`(eval Anything) -> Anything`

`(even? Integer) -> Boolean`

`(exit)`

`(false? Anything) -> Boolean`

`(filter Applicable List) -> List`

`(first List) -> Anything`

`(flip Applicable) -> Applicable`

`(float? Anything) -> Boolean`

`(foreign-value? Anything) -> Boolean`

`(function? Anything) -> Boolean`

`(gc)`

`(gensym) -> Symbol`

`(get Table Anything) -> Anything`

`(identity Anything) -> Anything`

`(input-stream? Anything) -> Boolean`

`(instance-of? Anything Type) -> Boolean`

`(integer? Anything) -> Boolean`

`(iostream? Anything) -> Boolean`

`(join-strings Text List) -> Text`

`(keys Table) -> List`

`(last List) -> Anything`

`(left Pair) -> Anything`

`(length List) -> Integer`

`(list &) -> List`

`(list? Anything) -> Boolean`

`(load Text) -> Anything`

`(map Applicable List) -> List`

`(merge Table Table) -> Table`

`(newline)`

`(next-last List) -> Anything`

`(nothing? Anything) -> Boolean`

`(odd? Integer) -> Boolean`

`(output-stream? Anything) -> Boolean`

`(partial Applicable &) -> Applicable`

`(print Anything)`

`(put Table Anything Anything) -> Table`

`(quit)`

`(random Integer) -> Integer`

`(read) -> Anything`

`(read-file Text) -> Text`

`(read-line InputStream) -> Text`

`(read-lines Text) -> List`

`(read-text Text) -> Anything`

`(reduce Applicable Anything List) -> Anything`

`(rest List) -> List`

`(right Pair) -> Anything`

`(room)`

`(second List) -> Anything`

`(show Anything) -> Text`

`(singleton? Anything) -> Boolean`

`(some? Applicable List) -> Anything`

`(something? Anything) -> Boolean`

`(split-string Text Character) -> List`

`(table? Anything) -> Boolean`

`(take Integer List) -> List`

`(take-by Integer Integer List) -> List`

`(text? Anything) -> Boolean`

`(true? Anything) -> Boolean`

`(uuid) -> Symbol`

`(vals Table) -> List`

`(version) -> Text`

`(write Anything)`

