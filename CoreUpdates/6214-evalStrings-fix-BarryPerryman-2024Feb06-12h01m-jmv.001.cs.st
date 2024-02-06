'From Cuis6.3 [latest update: #6213] on 6 February 2024 at 12:01:40 pm'!

!Array methodsFor: 'converting' stamp: 'BAP 2/6/2024 12:01:10'!
evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"

    ^ self collect: [:each |  | it |
        it := each.
        each == #true ifTrue: [it := true].
		      each == #false ifTrue: [it := false].
        each == #nil ifTrue: [it := nil].
        each isString ifTrue: [
		it := Compiler evaluate: each].
        each class == Array ifTrue: [it := it evalStrings].
        it]! !

