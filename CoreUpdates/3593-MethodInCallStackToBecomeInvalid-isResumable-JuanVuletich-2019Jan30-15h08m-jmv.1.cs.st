'From Cuis 5.0 of 7 November 2016 [latest update: #3592] on 30 January 2019 at 3:09:13 pm'!
!classDefinition: #MethodInCallStackToBecomeInvalid category: #'Exceptions Kernel'!
Exception subclass: #MethodInCallStackToBecomeInvalid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!MethodInCallStackToBecomeInvalid methodsFor: 'as yet unclassified' stamp: 'jmv 1/30/2019 15:04:38'!
defaultAction

	self noHandler! !


!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 1/30/2019 15:02:54'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| selfMethod otherObjectMethod selector contextReceiver |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				contextReceiver _ context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					selector _ context method selector.
					selfMethod _ self class lookupSelector: selector.
					otherObjectMethod _ otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: contextReceiver class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!Object methodsFor: 'system primitives' stamp: 'jmv 1/30/2019 15:02:40'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

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
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 1/30/2019 15:02:48'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

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
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!ClassDescription methodsFor: 'initialization' stamp: 'jmv 1/30/2019 15:02:33'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| newMethod oldMethod selector |
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				selector _ context method selector.
				oldMethod _ oldClass lookupSelector: selector.
				newMethod _ self lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					MethodInCallStackToBecomeInvalid
						signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
		runningProcessSearchStart: nil.
	self updateInstances: oldClass allInstances asArray from: oldClass isMeta: self isMeta.
	^nil

"This attempts to crash the VM by stepping off the end of an instance.
 As the doctor says, do not do this."
"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock value"! !

