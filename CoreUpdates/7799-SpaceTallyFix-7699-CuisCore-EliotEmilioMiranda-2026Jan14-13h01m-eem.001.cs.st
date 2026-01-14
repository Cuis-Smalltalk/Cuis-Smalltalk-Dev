'From Cuis7.5 [latest update: #7798] on 14 January 2026 at 12:56:46 pm'!

!ProtoObject class methodsFor: 'aux' stamp: 'jmv 5/Jul/2024 15:24:22'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self class bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	self isBytes ifTrue: [ ^1 ].
	self isDoubleBytes ifTrue: [ ^2 ].
	self isWords ifTrue: [ ^4 ].
	self isDoubleWords ifTrue: [ ^8 ].
	^Smalltalk wordSize.! !

!methodRemoval: ArrayedCollection class #bytesPerBasicElement stamp: 'eem 14/Jan/2026 12:56:02'!
ArrayedCollection class removeSelector: #bytesPerBasicElement!
!methodRemoval: AffineTransformation class #bytesPerBasicElement stamp: 'eem 14/Jan/2026 12:55:59'!
AffineTransformation class removeSelector: #bytesPerBasicElement!
