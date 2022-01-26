'From Cuis 6.0 [latest update: #5048] on 26 January 2022 at 4:50:38 pm'!

!Object methodsFor: 'system primitives' stamp: 'jmv 1/26/2022 16:49:44'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:
	"

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. #becomeForward: aborted.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 1/26/2022 16:50:06'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:copyHash:
	"

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. #becomeForward:copyHash: aborted.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!Array methodsFor: 'converting' stamp: 'jmv 1/26/2022 16:48:48'!
elementsForwardIdentityTo: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.

	 The identityHashes remain with the pointers in us rather than with the objects so that
	 the objects in the receiver should still be properly indexed in any existing hashed
	 structures after the mutation.

	Warning: This means that the identityHash of objects in otherArray will change. This
	is most likely a problem if those objects are already in any identityHashed collection!!
	See #elementsForwardIdentityTo:copyHash:.
	They should be #rehash 'ed.

	Additionally, any hashed (not identityHashed) collection with elements in the receiver
	will most likely break if the #hash message gives now a different answer!!
	They should be #rehash 'ed.

	Warning. This is a dangerous operation and it could lead to a crash if some object in
	receiver or argument is receiver of a method currently in execution.
	See #anyReceiverInStackIn: See senders."

	<primitive: 72 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[self error: 'The virtual machine is out-of-date.  Please upgrade.'].
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'jmv 1/26/2022 16:47:28'!
elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.

	 If copyHash is true, the identityHashes remain with the pointers in rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.	
	Warning: This means that the identityHash of objects in otherArray will change. This
	is most likely a problem if those objects are already in any identityHashed collection!!

	If copyHash is false, then the hashes of the objects in otherArray remain unchanged.
	If you know what you're doing this may indeed be what you want.
	Warning: This means that any identityHashed collection with elements in otherArray
	will not be broken, but any identityHashed collection with elements in self will be!!
	They should be #rehash 'ed.

	Additionally, any hashed (not identityHashed) collection with elements in the receiver
	will most likely break if the #hash message gives now a different answer!!
	They should be #rehash 'ed.

	Warning. This is a dangerous operation and it could lead to a crash if some object in
	receiver or argument is receiver of a method currently in execution.
	See #anyReceiverInStackIn: See senders."

	<primitive: 249 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	self primitiveFailed! !

