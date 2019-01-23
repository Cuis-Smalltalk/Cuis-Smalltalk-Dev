'From Cuis 5.0 of 7 November 2016 [latest update: #3584] on 21 January 2019 at 12:44:58 pm'!

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:43:41'!
anyReceiverInStackIn: anArray orIn: anotherArray runningProcessSearchStart: aContextOrNil
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: {} orIn: { self runningWorld } runningProcessSearchStart: nil
	Processor anyReceiverInStackIn: {} orIn: { Object new } runningProcessSearchStart: nil
	"
	self
		processesDo: [ :p | ] withStackFramestDo: [ :p :c | | r |
			r _ c receiver.
			(anArray statePointsTo: r)
				ifTrue: [ ^ true ].
			(anotherArray statePointsTo: r)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: aContextOrNil.
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:43:55'!
anyReceiverInStackIn: anArray runningProcessSearchStart: aContextOrNil
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: { self runningWorld } runningProcessSearchStart: nil
	Processor anyReceiverInStackIn: { Object new } runningProcessSearchStart: nil
	"
	self
		processesDo: [ :p | ] withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: aContextOrNil.
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:25:46'!
processesDo: aBlock withStackFramestDo: twoArgBlock runningProcessSearchStart: aContextOrNil
	"Iterate over processes that can run.
	For each process, iterate over stack frames (i.e. contexts)."
	"
	Processor
		processesDo: [ :p |
			'--------------' print.
			p print. 
			'--------------' print ]
		withStackFramestDo: [ :p :c |
			('        ', c printString) print ].
	"
	self
		processesWithTopContextDo: [ :process :topContext | | context |
			aBlock value: process.
			context _ topContext.
			[ context notNil ] whileTrue: [
				twoArgBlock value: process value: context.
				context _ context sender ]]
		runningProcessSearchStart: aContextOrNil! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:25:07'!
processesWithTopContextDo: aBlock runningProcessSearchStart: aContextOrNil
	"Iterate over processes that can run. Include top context in block arguments."
	"
	Processor processesWithTopContextDo: [ :p :c | p print. ('------->', c printString) print ].
	"
	self processesDo: [ :process |
		aBlock
			value: process
			value: (process isRunning ifTrue: [ aContextOrNil ifNil: [thisContext] ] ifFalse: [ process suspendedContext ]) ]! !


!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 1/21/2019 12:37:38'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| oldObjects newObjects |
	oldObjects _ { self }.
	newObjects _ { otherObject }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects orIn: newObjects runningProcessSearchStart: thisContext sender) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Either receiver or argument has some method currently in execution.']].
	oldObjects elementsExchangeIdentityWith: newObjects! !


!Object methodsFor: 'system primitives' stamp: 'jmv 1/21/2019 12:37:46'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	| oldObjects |
	oldObjects _ { self }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects runningProcessSearchStart: thisContext sender) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Receiver has some method currently in execution.']].
	oldObjects elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 1/21/2019 12:37:50'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	| oldObjects |
	oldObjects _ { self }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects runningProcessSearchStart: thisContext sender) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Receiver has some method currently in execution.']].
	oldObjects
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:34:33'!
anyReceiverInStackIn: anArray
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: { self runningWorld }
	Processor anyReceiverInStackIn: { Object new }
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/21/2019 12:34:49'!
anyReceiverInStackIsKindOf: aClass
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' is a (sub)instance of aClass"
	"
	Processor anyReceiverInStackIsKindOf: Morph
	Processor anyReceiverInStackIsKindOf: DifferenceFinder
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(c receiver isKindOf: aClass)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false! !

!methodRemoval: ProcessorScheduler #anyReceiverInStackIn:orIn:!
ProcessorScheduler removeSelector: #anyReceiverInStackIn:orIn:!
!methodRemoval: ProcessorScheduler #processesDo:withStackFramestDo:!
ProcessorScheduler removeSelector: #processesDo:withStackFramestDo:!
!methodRemoval: ProcessorScheduler #processesWithTopContextDo:!
ProcessorScheduler removeSelector: #processesWithTopContextDo:!
