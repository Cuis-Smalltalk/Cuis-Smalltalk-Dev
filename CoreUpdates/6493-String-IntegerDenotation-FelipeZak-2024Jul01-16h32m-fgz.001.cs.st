'From Cuis7.1 [latest update: #6455] on 1 July 2024 at 4:34:08 pm'!

!String methodsFor: 'testing' stamp: 'fgz 7/1/2024 16:33:58'!
isIntegerDenotation
	"Answer true if the receiver denotes an integer number."
	
	^(self size > 1 and: [
		self first = $- and: [ (self findFirst: [:each | each isDigit not ] startingAt: 2) = 0 ]
			]) or: [ self size > 0 and: [ self allSatisfy: [:each | each isDigit ] ] ]

! !

