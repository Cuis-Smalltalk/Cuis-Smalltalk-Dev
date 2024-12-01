'From Cuis7.1 [latest update: #6880] on 1 December 2024 at 11:33:12 am'!

!Character methodsFor: 'accessing' stamp: 'jmv 12/1/2024 11:32:58'!
codePoint
	"Code points are the numbers assigned by the Unicode Consortium to every character in every writing system."
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $€ codePoint hex = '16r20AC'.
	"
	<primitive: 171>
	^self primitiveFailed! !

