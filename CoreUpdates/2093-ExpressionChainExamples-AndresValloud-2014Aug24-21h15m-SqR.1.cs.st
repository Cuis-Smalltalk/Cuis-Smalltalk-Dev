'From Cuis 4.2 of 25 July 2013 [latest update: #1962] on 27 August 2014 at 10:25:56.937538 am'!

!Parser class methodsFor: 'accessing' stamp: 'SqR 8/25/2014 10:06'!
chainExamplesDoubleColon

	1 bitShift: 3 :: + 5.  "13"

	3 + 4 :: negated + 2 :: negated - 2 bitShift: 2 :: bitShift: -1.  "6"

	1 = 2 and: [3 = 4] :: ifTrue: [5] ifFalse: [6].  "6"

	1 = 1
		:: and: [2 = 2]
		:: and: [3 = 3]
		:: and: [4 = 4]
		:: ifTrue: [#yes] ifFalse: [#bummer].  "#yes"

	1 = 1 :: = true.  "true"

	1 = 1 :: ifTrue: [2] ifFalse: [3] :: + 1 ; + 10.  "12"

	1 bitShift: 3 :: + 2 bitShift: -1.  "5"

	1 to: 10 by: 3 :: size.  "4"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x].  "22"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x] :: bitShift: -1 :: - 1.  "10"

	CompiledMethod allInstances
		:: reject: [:x | x getSourceFromFile isNil]
		:: collect: [:x | x getSourceFromFile -> x]
		:: collect: [:x | x key occurrencesOf: $( :: + (x key occurrencesOf: $)) :: -> x key -> x value]
		:: asSortedCollection: [:x :y | x > y]
		:: inspect.! !
		