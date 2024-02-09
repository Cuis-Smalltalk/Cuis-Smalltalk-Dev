'From Cuis6.3 [latest update: #6156] on 8 February 2024 at 8:38:57 pm'!
!Array methodsFor: 'converting' stamp: 'tb 2/8/2024 20:24:02'!
evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"
   
  
    ^ self collect: [:each |  
	each isSymbol 
		ifTrue: [each == #true 
			ifTrue: [true]
			ifFalse: [each == #false 
				ifTrue: [false]
				ifFalse:[each == #nil 
					ifTrue: [nil]
					ifFalse: [each]]]] 
		ifFalse: [each isString 
				ifTrue: [Compiler evaluate: each]
				ifFalse: [each isArray 
					ifTrue: [each evalStrings]
					ifFalse:[each]]]
        ]! !
