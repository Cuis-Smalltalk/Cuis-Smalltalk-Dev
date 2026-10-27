'From Cuis7.9 [latest update: #8295] on 27 October 2026 at 2:46:18 pm'!

!ProtoObject methodsFor: 'printing' stamp: 'jmv 27/Oct/2026, 5:30:24 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printString
	"Answer a String whose characters are a description of the receiver. 
	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.
	This method is redefined in Object."

	^ String streamContents: [:str | self printOn: str ]! !


!Number methodsFor: 'printing' stamp: 'laza 29/Mar/2004, 3:53:00 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printOn: aStream
	self printOn: aStream base: 10! !

