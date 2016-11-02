'From Cuis 4.2 of 25 July 2013 [latest update: #2963] on 1 November 2016 at 10:08:34 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 11/1/2016 22:06:26'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	"

	| nv |
	nv _ self numericValue.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1! !

!Character methodsFor: 'testing' stamp: 'jmv 11/1/2016 22:06:16'!
isDigit
	"Answer whether the receiver is a digit."

	^ self numericValue between: 48 and: 57! !

!Character methodsFor: 'testing' stamp: 'jmv 11/1/2016 22:06:09'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12) statePointsTo: self numericValue! !

!Character methodsFor: 'testing' stamp: 'jmv 11/1/2016 22:06:06'!
isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."

	^ #(32 9 10 13 12) statePointsTo: self numericValue! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/1/2016 21:52:42'!
cr
	"Answer the Character representing a carriage return."

	^ ($)! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/1/2016 21:52:37'!
lf
	"Answer the Character representing a linefeed."

	^ ($
)! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/1/2016 21:52:33'!
space
	"Answer the Character representing a space."

	^ ($ )! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 11/1/2016 21:52:28'!
tab
	"Answer the Character representing a tab."

	^ ($	)! !

