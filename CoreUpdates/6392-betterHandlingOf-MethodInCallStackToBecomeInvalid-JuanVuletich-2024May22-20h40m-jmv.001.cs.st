'From Cuis7.1 [latest update: #6397] on 22 May 2024 at 8:50:32 pm'!

!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 5/22/2024 20:50:10'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| selfMethod otherObjectMethod selector contextReceiver errorMessage |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				contextReceiver := context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					selector := context method selector.
					selfMethod := self class lookupSelector: selector.
					otherObjectMethod := otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						errorMessage := contextReceiver class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#become: aborted.' print.
							Error signal: errorMessage, ' #become: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!Object methodsFor: 'system primitives' stamp: 'jmv 5/22/2024 20:49:54'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:
	"

	| newMethod oldMethod selector errorMessage |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward: aborted.' print.
							Error signal: errorMessage, ' #becomeForward: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 5/22/2024 20:50:04'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger.

	Please see comment at #elementsForwardIdentityTo:copyHash:
	"

	| newMethod oldMethod selector errorMessage |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					selector := context method selector.
					oldMethod := self class lookupSelector: selector.
					newMethod := otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						errorMessage := self class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#becomeForward:copyHash: aborted.' print.
							Error signal: errorMessage, ' #becomeForward:copyHash: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!ClassDescription methodsFor: 'initialization' stamp: 'jmv 5/22/2024 20:45:28'!
canUpdateFrom: oldClass
	"If some compiled method that will become invalid is currently being executed,
	the shape conversion of instances is extremely insecure.
	Answer if migration of instances looks safe"
	| answer newMethod oldMethod selector newActualClass oldActualClass |
	answer := true.
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				oldActualClass := context receiver class.
				self withAllSubclassesDo: [ :c |
					c name = oldActualClass name ifTrue: [
						newActualClass := c ]].
				selector := context method selector.
				oldMethod := oldActualClass lookupSelector: selector.
				newMethod := newActualClass lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					('Class reshaping: ', context receiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid.') print.
					answer := false ].
				]]
		runningProcessSearchStart: nil.
	^answer! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/22/2024 20:45:17'!
recompile: force from: oldClass to: newClass mutate: forceMutation
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	| compilationSuccess compilationErrorDescription |
	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex := 0.
	maxClassIndex := oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].
	"Recompile oldClass to newClass"
	self informUserDuring: [
		compilationSuccess := true.
		[self createHierarchy: oldClass to: newClass] on: Error do: [ :exception |
			"If compilation failed, don't continue!!"
			compilationErrorDescription := exception printString.
			compilationSuccess := false ].
	].

	compilationSuccess ifFalse: [
		oldClass superclass removeSubclass: newClass.
		Smalltalk garbageCollect.
		Error signal: compilationErrorDescription, '. Please see Transcript.'.
		^ nil ].

	(newClass canUpdateFrom: oldClass) ifFalse: [
		MethodInCallStackToBecomeInvalid signal ifFalse: [
			oldClass superclass removeSubclass: newClass.
			Smalltalk garbageCollect.
			'Class reshaping aborted.' print.
			Error signal: oldClass name, 
				' has some subInstance running a CompiledMethod that would become invalid. Please see Transcript.'.
			^nil ]].

	"Mutate oldClass to newClass"
	self informUserDuring: [
		self mutateInstances: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

