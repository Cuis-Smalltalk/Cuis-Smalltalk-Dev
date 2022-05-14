'From Cuis 6.0 [latest update: #5163] on 14 May 2022 at 9:12:38 am'!

!Collection methodsFor: 'accessing' stamp: 'len 5/14/2022 09:12:04'!
atRandom: aGenerator 
	"Answer a random element of the receiver.  Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator.  Causes an error if self has no  
	elements.
	WARNING: This implementation cannot guarantee reproducibility.
	Calling twice with generators setup with the same seed can produce
	different results (for example if the collection is rehashed between calls).
	If you need reproducibility, you should convert the collection to a sequenceable
	collection first."
	| n |
	self emptyCheck.
	n _ self size atRandom: aGenerator.
	self do: [:each| (n _ n - 1) = 0 ifTrue: [^ each]].
	self error: 'collection invariants broken'! !


!Bag methodsFor: 'accessing' stamp: 'len 5/14/2022 09:11:46'!
atRandom: aGenerator 
	"Answer a random element of the receiver.  Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator.  Causes an error if self has no  
	elements.
	WARNING: This implementation cannot guarantee reproducibility.
	Calling twice with generators setup with the same seed can produce
	different results (for example if the collection is rehashed between calls).
	If you need reproducibility, you should convert the collection to a sequenceable
	collection first."
	| n |
	self emptyCheck.
	n _ self size atRandom: aGenerator.
	self withOccurrencesDo: [:each :times| (n _ n - times) <= 0 ifTrue: [^ each]].
	self error: 'collection invariants broken'! !


!Set methodsFor: 'accessing' stamp: 'len 5/14/2022 09:11:53'!
atRandom: aGenerator 
	"Answer a random element of the receiver.  Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator.  Causes an error if self has no  
	elements.
	WARNING: This implementation cannot guarantee reproducibility.
	Calling twice with generators setup with the same seed can produce
	different results (for example if the collection is rehashed between calls).
	If you need reproducibility, you should convert the collection to a sequenceable
	collection first."
	self emptyCheck.
	[(array atRandom: aGenerator) ifNotNil: [:element| ^ element]] repeat! !

