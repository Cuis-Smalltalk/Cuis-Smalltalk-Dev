'From Cuis6.3 [latest update: #6208] on 6 February 2024 at 9:49:53 am'!

!String class methodsFor: 'instance creation' stamp: 'jmv 2/4/2024 21:05:06'!
new: sizeRequested 
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  ! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 17:43:28'!
new: sizeRequested
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  ! !


!Bag class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:07:20'!
new
	^ self basicNew setContents: self contentsClass new! !


!SharedQueue class methodsFor: 'instance creation' stamp: 'jmv 2/5/2024 18:32:59'!
new: size

	self error: 'Instances of ', self name, ' can not be created with the #new: message. Just call #new instead.'! !

!methodRemoval: MethodDictionary class #new: stamp: 'jmv 2/6/2024 09:48:05'!
MethodDictionary class removeSelector: #new:!
!methodRemoval: Set class #new: stamp: 'jmv 2/6/2024 09:48:05'!
Set class removeSelector: #new:!
!methodRemoval: Bag class #new: stamp: 'jmv 2/6/2024 09:48:05'!
Bag class removeSelector: #new:!
!methodRemoval: OrderedCollection class #new: stamp: 'jmv 2/6/2024 09:48:05'!
OrderedCollection class removeSelector: #new:!
!methodRemoval: Heap class #new: stamp: 'jmv 2/6/2024 09:48:05'!
Heap class removeSelector: #new:!
