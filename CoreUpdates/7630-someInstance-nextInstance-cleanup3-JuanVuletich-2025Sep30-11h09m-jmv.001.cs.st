'From Cuis7.5 [latest update: #7628] on 30 September 2025 at 11:10:01 am'!

!ProtoObject methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 9/30/2025 11:09:34'!
nextInstance
	"Answer the next instance after the receiver in the enumeration of all instances of this class.
	Fails if all instances have been enumerated.
	See Object>>#someInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.
	
	Warning: Spur VMs can not guarantee that every instance is visited and that it is visited only once.
		Use of this primitive is strongly discouraged."

	<primitive: 78>
	^nil! !


!Behavior methodsFor: 'instances' library: #MinimalKernel01 stamp: 'jmv 9/30/2025 11:09:43'!
someInstance
	"Answer the first instance of this receiver.
	Fails if there are none.
	See Object>>#nextInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.
	
	Warning: Spur VMs can not guarantee that every instance is visited and that it is visited only once.
		Use of this primitive is strongly discouraged."

	<primitive: 77>
	^nil! !

