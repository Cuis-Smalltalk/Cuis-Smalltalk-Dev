'From Cuis7.3 [latest update: #6994] on 21 January 2025 at 8:23:18 am'!

!EphemeronFinalizationProcess class methodsFor: 'private - mourning' stamp: 'jmv 1/21/2025 08:19:08'!
primitiveFetchMourner
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withNewLines].
	^nil! !

!methodRemoval: String #withCRs stamp: 'jmv 1/21/2025 08:22:58'!
String removeSelector: #withCRs!
