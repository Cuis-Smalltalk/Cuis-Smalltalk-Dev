'From Cuis 6.0 [latest update: #5141] on 9 May 2022 at 10:43:32 am'!

!Collection methodsFor: 'accessing' stamp: 'len 5/9/2022 07:50:44'!
atRandom: aGenerator 
	"Answer a random element of the receiver. Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator. Causes an error if self has no  
	elements."
	| n |
	n _ self size atRandom: aGenerator.
	self do: [:each| (n _ n - 1) = 0 ifTrue: [^ each]].
	self errorEmptyCollection! !


!Bag methodsFor: 'accessing' stamp: 'len 5/9/2022 10:34:52'!
atRandom: aGenerator 
	"Answer a random element of the receiver. Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator. Causes an error if self has no  
	elements."
	| n |
	n _ self size atRandom: aGenerator.
	self withOccurrencesDo: [:each :times| (n _ n - times) <= 0 ifTrue: [^ each]].
	self errorEmptyCollection! !


!Collection methodsFor: 'testing' stamp: 'len 5/9/2022 07:31:47'!
identityIncludes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:any| any == anObject]! !

