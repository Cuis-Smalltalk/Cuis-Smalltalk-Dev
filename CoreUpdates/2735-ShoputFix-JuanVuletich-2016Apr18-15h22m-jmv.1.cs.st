'From Cuis 4.2 of 25 July 2013 [latest update: #2734] on 18 April 2016 at 3:23:19.302832 pm'!

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 4/18/2016 15:22'!
last3Ranges
	| r s lastI |
	s _ ranges size.
	s = 0 ifTrue: [ ^ #(nil nil nil) ].
	r _ ranges last.
	lastI _ r rangeType = #excessCode
		ifTrue: [ s - 1 ]
		ifFalse: [ s].
	^{ 
		(lastI > 2 ifTrue: [ranges at: lastI-2]).
		(lastI > 1 ifTrue: [ranges at: lastI-1]).
		(lastI > 0 ifTrue: [ranges at: lastI])
	}! !

