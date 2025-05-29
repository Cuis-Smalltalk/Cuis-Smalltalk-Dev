'From Cuis7.3 [latest update: #7254] on 29 May 2025 at 9:15:57 am'!

!ProtoObject methodsFor: '[mk] system primitives' stamp: 'jmv 5/29/2025 09:15:37'!
nextInstance
	"Answer the next instance after the receiver in the enumeration of all instances of this class.
	Fails if all instances have been enumerated.
	See Object>>#someInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.

	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 78>
	^nil! !


!Behavior methodsFor: '[mk] instances' stamp: 'jmv 5/29/2025 09:15:16'!
someInstance
	"Answer the first instance of this receiver.
	Fails if there are none.
	See Object>>#nextInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.

	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^nil! !

