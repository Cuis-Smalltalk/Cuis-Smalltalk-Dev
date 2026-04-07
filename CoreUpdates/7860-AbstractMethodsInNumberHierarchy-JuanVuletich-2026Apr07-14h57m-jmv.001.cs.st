'From Cuis7.7 [latest update: #7858] on 7 April 2026 at 3:03:09 pm'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 7/Apr/2026 14:57:32'!
primSqrt
	^self subclassResponsibility! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 7/Apr/2026 14:58:49'!
timesTwoPower: anInteger
	^self subclassResponsibility! !


!Integer methodsFor: 'system primitives' stamp: 'jmv 7/Apr/2026 15:00:05'!
digitAt: n 
	"Answer the value of an indexable field in the receiver.  LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds."
	self subclassResponsibility! !

!Integer methodsFor: 'system primitives' stamp: 'jmv 7/Apr/2026 15:01:26'!
digitAt: n put: value 
	"Fails. The digits of a small integer can not be modified."

	self error: 'You can''t store in a ', self class name! !

!Integer methodsFor: 'system primitives' stamp: 'jmv 7/Apr/2026 15:00:18'!
digitLength
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."
	self subclassResponsibility! !

!methodRemoval: SmallInteger #digitAt:put: stamp: 'jmv 7/Apr/2026 15:01:47'!
SmallInteger removeSelector: #digitAt:put:!
