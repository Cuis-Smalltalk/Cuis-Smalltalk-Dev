'From Cuis 4.2 of 25 July 2013 [latest update: #1902] on 24 December 2013 at 12:47:28.836402 pm'!

!Integer methodsFor: 'truncation and round off' stamp: 'sqr 12/21/2013 17:55'!
ceiling 
	"Refer to the comment in Number|ceiling."

	^self! !

!Integer methodsFor: 'truncation and round off' stamp: 'sqr 12/21/2013 17:56'!
floor 
	"Refer to the comment in Number|floor."

	^self! !

!Integer methodsFor: 'truncation and round off' stamp: 'sqr 12/21/2013 17:56'!
rounded 
	"Refer to the comment in Number|rounded."

	^self! !

!Integer methodsFor: 'truncation and round off' stamp: 'sqr 12/21/2013 17:56'!
truncated 
	"Refer to the comment in Number|truncated."

	^self! !


!SmallInteger methodsFor: 'copying' stamp: 'sqr 12/21/2013 17:44'!
clone

	^self! !

!SmallInteger methodsFor: 'copying' stamp: 'sqr 12/21/2013 17:49'!
shallowCopy

	^self! !

!SmallInteger methodsFor: 'printing' stamp: 'sqr 12/21/2013 17:44'!
destinationBuffer: digitLength
	^ LargePositiveInteger new: digitLength! !

!methodRemoval: SmallInteger #as31BitSmallInt!
SmallInteger removeSelector: #as31BitSmallInt!
!methodRemoval: SmallInteger #isLarge!
SmallInteger removeSelector: #isLarge!
!methodRemoval: LargePositiveInteger #as31BitSmallInt!
LargePositiveInteger removeSelector: #as31BitSmallInt!
!methodRemoval: LargePositiveInteger #isLarge!
LargePositiveInteger removeSelector: #isLarge!
!methodRemoval: Integer #asPrecedenceName!
Integer removeSelector: #asPrecedenceName!
