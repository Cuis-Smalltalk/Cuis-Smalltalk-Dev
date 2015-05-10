'From Cuis 4.2 of 25 July 2013 [latest update: #2312] on 8 May 2015 at 4:02:06.835299 pm'!

!OrderedCollection methodsFor: 'adding' stamp: 'ul 4/12/2015 23:55'!
addAllLast: aCollection 
	"Add each element of aCollection at the end of me. Answer aCollection."

	^aCollection do: [ :each | self addLast: each ]! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 5/8/2015 16:02'!
selectors
	"Answer a Set of all the message selectors specified in the receiver's 
	method dictionary."

	^ self methodDict keys
	"
	Point selectors
	"! !


!CodePackage methodsFor: 'listing' stamp: 'jmv 5/8/2015 16:01'!
selectors

	^ self methods collect: [ :ea | ea methodSymbol ]! !


!PseudoClass methodsFor: 'methods' stamp: 'jmv 5/8/2015 16:01'!
selectors

	^ self sourceCode keys! !

