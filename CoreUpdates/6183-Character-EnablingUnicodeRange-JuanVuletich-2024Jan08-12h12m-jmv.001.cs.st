'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 12:14:04 pm'!

!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:13:35'!
codePoint: anInteger
	"Answer the Character whose value is anInteger.
	
	self assert: (Character codePoint: 16r41) = $A.
	self assert: (Character codePoint: 16r20AC) = $€.
	"

	<primitive: 170>
	^self primitiveFailed! !

