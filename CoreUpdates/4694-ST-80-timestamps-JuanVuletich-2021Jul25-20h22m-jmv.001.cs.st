'From Cuis 5.0 [latest update: #4690] on 25 July 2021 at 8:28:11 pm'!

!LargePositiveInteger methodsFor: 'system primitives' stamp: 'ST-80 5/31/1983 9:10:35'!
digitAt: index put: value 
	"Store the second argument (value) in the indexable field of the receiver 
	indicated by index.  Fail if the value is negative or is larger than 255.  Fail if the 
	index is not an Integer or is out of bounds.  Answer with the value that was 
	stored.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value! !

!LargePositiveInteger methodsFor: 'system primitives' stamp: 'ST-80 5/31/1983 9:10:35'!
digitLength
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed! !

