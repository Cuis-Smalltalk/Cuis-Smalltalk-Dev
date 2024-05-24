'From Cuis7.1 [latest update: #6393] on 24 May 2024 at 3:31:30 pm'!

!LargePositiveInteger methodsFor: 'testing' stamp: 'ul 1/22/2012 23:56'!
isPowerOfTwo
	"Return true if the receiver is an integral power of two. Optimized version."
	
	| size |
	1 to: (size := self digitLength) do: [ :index |
		| digit |
		(digit := self digitAt: index) = 0 ifFalse: [ 
			^size = index and: [ digit isPowerOfTwo ] ] ].
	^false! !


!LargeNegativeInteger methodsFor: 'testing' stamp: 'ul 1/22/2012 23:57'!
isPowerOfTwo
	"Return true if the receiver is an integral power of two. Optimized version."
	
	^false! !


!Integer methodsFor: 'testing' stamp: 'ul 1/22/2012 23:56'!
isPowerOfTwo
	"Return true if the receiver is an integral power of two."
	
	^self > 0 and: [ (self bitAnd: self - 1) = 0 ]! !

