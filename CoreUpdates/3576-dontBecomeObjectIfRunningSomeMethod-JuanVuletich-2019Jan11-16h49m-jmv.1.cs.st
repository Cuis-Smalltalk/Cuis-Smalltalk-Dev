'From Cuis 5.0 of 7 November 2016 [latest update: #3575] on 11 January 2019 at 4:57:40 pm'!
!classDefinition: #AttemptToMutateObjectInCallStack category: #'Exceptions Kernel'!
Error subclass: #AttemptToMutateObjectInCallStack
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!AttemptToMutateObjectInCallStack commentStamp: '<historical>' prior: 0!
A become operation tries to mutate an object that is the receiver ('self') in a method currently in execution, and part of the stack of calls of some process.!


!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/11/2019 11:08:09'!
anyReceiverInStackIn: anArray
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: { self runningWorld }
	Processor anyReceiverInStackIn: { Object new }
	"
	self processesDo: [ :p | ] withStackFramestDo: [ :p :c |
		(anArray statePointsTo: c receiver)
			ifTrue: [ ^ true ]].
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/11/2019 11:11:48'!
anyReceiverInStackIn: anArray orIn: anotherArray
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: {} orIn: { self runningWorld }
	Processor anyReceiverInStackIn: {} orIn: { Object new }
	"
	self processesDo: [ :p | ] withStackFramestDo: [ :p :c | | r |
		r _ c receiver.
		(anArray statePointsTo: r)
			ifTrue: [ ^ true ].
		(anotherArray statePointsTo: r)
			ifTrue: [ ^ true ]].
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/9/2019 16:02:19'!
anyReceiverInStackIsKindOf: aClass
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' is a (sub)instance of aClass"
	"
	Processor anyReceiverInStackIsKindOf: Morph
	Processor anyReceiverInStackIsKindOf: DifferenceFinder
	"
	self processesDo: [ :p | ] withStackFramestDo: [ :p :c |
		(c receiver isKindOf: aClass)
			ifTrue: [ ^ true ]].
	^ false! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/9/2019 15:54:20'!
processesDo: aBlock
	"Iterate over processes that can run"
	"
	Processor processesDo: [ :p | p print ].
	"
	Process allSubInstancesDo: [ :p |
		p isTerminated ifFalse: [
			aBlock value: p ]]! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/9/2019 15:59:16'!
processesDo: aBlock withStackFramestDo: twoArgBlock
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
	self processesWithTopContextDo: [ :process :topContext | | context |
		aBlock value: process.
		context _ topContext.
		[ context notNil ] whileTrue: [
			twoArgBlock value: process value: context.
			context _ context sender ]]! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 1/9/2019 15:54:54'!
processesWithTopContextDo: aBlock
	"Iterate over processes that can run. Include top context in block arguments."
	"
	Processor processesWithTopContextDo: [ :p :c | p print. ('------->', c printString) print ].
	"
	self processesDo: [ :process |
		aBlock
			value: process
			value: (process isRunning ifTrue: [ thisContext ] ifFalse: [ process suspendedContext ]) ]! !


!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 1/11/2019 16:52:15'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| oldObjects newObjects |
	oldObjects _ { self }.
	newObjects _ { otherObject }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects orIn: newObjects) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Either receiver or argument has some method currently in execution.']].
	oldObjects elementsExchangeIdentityWith: newObjects! !


!Object methodsFor: 'system primitives' stamp: 'jmv 1/11/2019 16:52:24'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	| oldObjects |
	oldObjects _ { self }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Receiver has some method currently in execution.']].
	oldObjects elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 1/11/2019 16:52:33'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	| oldObjects |
	oldObjects _ { self }.
	self class == otherObject class ifFalse: [
		(Processor anyReceiverInStackIn: oldObjects) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: 'Can not do #become: Receiver has some method currently in execution.']].
	oldObjects
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!Behavior methodsFor: 'private' stamp: 'jmv 1/11/2019 11:39:10'!
becomeCompact
	"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct index |

	Smalltalk isSpur ifTrue: [ ^ self halt: 'No Compact Classes support in Spur' ].
	self isWeak ifTrue:[ ^ self halt: 'You must not make a weak class compact' ].
	(Processor anyReceiverInStackIsKindOf: self)
		ifTrue: [ ^self halt: self name, ' has some (sub)instance with some method currently in execution.' ].
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name, ' is already compact'].
	index _ cct indexOf: nil
		ifAbsent: [^ self halt: 'compact class table is full'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format _ format + (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Purge any old instances"
	Smalltalk garbageCollect.! !

!Behavior methodsFor: 'private' stamp: 'jmv 1/11/2019 11:39:04'!
becomeUncompact
	| cct index |
	cct _ Smalltalk compactClassesArray.
	(index _ self indexIfCompact) = 0
		ifTrue: [^ self].
	(Processor anyReceiverInStackIsKindOf: self)
		ifTrue: [ ^self halt: self name, ' has some (sub)instance with some method currently in execution.' ].
	(cct includes: self)
		ifFalse: [^ self halt  "inconsistent state"].
	"Update instspec so future instances will not be compact"
	format _ format - (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Make sure there are no compact ones left around"
	Smalltalk garbageCollect.
	"Remove this class from the compact class table"
	cct at: index put: nil.
! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/11/2019 16:55:48'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil].
		(Processor anyReceiverInStackIsKindOf: oldClass) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: oldClass name, ' has some (sub)instance with some method currently in execution.'. ^nil ]].
	"See if we need a new subclass or not"
	needNew _ self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class"
	copyOfOldClass _ oldClass copy.
	newClass _ self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.
		
	newClass _ self recompile: false from: oldClass to: newClass mutate: false.
	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/11/2019 16:56:13'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Scanner new scanFieldNames: instVarString.
	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil].
		(Processor anyReceiverInStackIsKindOf: oldClass) ifTrue: [
			AttemptToMutateObjectInCallStack
				signal: oldClass name, ' has some (sub)instance with some method currently in execution.'. ^nil ]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category asSymbol.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: category].
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/11/2019 11:58:48'!
recompile: force from: oldClass to: newClass mutate: forceMutation
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex _ 0.
	maxClassIndex _ oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo:[:cl|
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].
	"Recompile and mutate oldClass to newClass"
	self informUserDuring:[
		self mutate: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 1/11/2019 16:56:52'!
moveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName
	"Move the given instVar from srcClass to dstClass"
	| instancesOk |
	(srcClass instVarNames includes: instVarName)
		ifFalse:[^self error: instVarName,' is not an instance variable of ', srcClass name].
	(prevInstVarName isNil or:[dstClass instVarNames includes: prevInstVarName])
		ifFalse:[^self error: prevInstVarName, 'is not an instance variable of', dstClass name].
	instancesOk _ false.
	(srcClass inheritsFrom: dstClass) ifTrue:[
		"Move the instvar up the hierarchy."
		(Processor anyReceiverInStackIsKindOf: dstClass) ifTrue: [ 
			AttemptToMutateObjectInCallStack
				signal: dstClass name, ' has some (sub)instance with some method currently in execution.'  ^false ].
		instancesOk _ true.
		(self validateClass: srcClass forMoving: instVarName upTo: dstClass)
			ifFalse:[^false].
	].
	(dstClass inheritsFrom: srcClass) ifTrue:[
		"Move the instvar down the hierarchy"
		(Processor anyReceiverInStackIsKindOf: srcClass) ifTrue: [ 
			AttemptToMutateObjectInCallStack
				signal: srcClass name, ' has some (sub)instance with some method currently in execution.'  ^false ].
		instancesOk _ true.
		(self validateClass: srcClass forMoving: instVarName downTo: dstClass)
			ifFalse:[^false].
	].
	instancesOk ifFalse: [	"disjunt hierarchies"
		(Processor anyReceiverInStackIsKindOf: dstClass) ifTrue: [ 
			AttemptToMutateObjectInCallStack
				signal: dstClass name, ' has some (sub)instance with some method currently in execution.'  ^false ].
		(Processor anyReceiverInStackIsKindOf: srcClass) ifTrue: [ 
			AttemptToMutateObjectInCallStack
				signal: srcClass name, ' has some (sub)instance with some method currently in execution.'  ^false ]].
	^self silentlyMoveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName! !


!Array methodsFor: 'converting' stamp: 'jmv 1/11/2019 11:10:01'!
elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

	<primitive: 128 error: ec>
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
		[| maxRequired |
		 "In Spur, two-way become may involve making each pair of objects into a forwarder into a copy of the other.
		 So if become fails with #'insufficient object memory', garbage collect, and if necessary, grow memory."
		 maxRequired := (self detectSum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize])
						+ (otherArray detectSum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize]).
		 (Smalltalk garbageCollectMost < maxRequired
		  and: [Smalltalk garbageCollect < maxRequired]) ifTrue:
			[Smalltalk growMemoryByAtLeast: maxRequired].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'jmv 1/11/2019 11:09:50'!
elementsForwardIdentityTo: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 The identityHashes remain with the pointers rather than with the objects so that
	 the objects in this array should still be properly indexed in any existing hashed
	 structures after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

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

!Array methodsFor: 'converting' stamp: 'jmv 1/11/2019 11:09:40'!
elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 If copyHash is true, the identityHashes remain with the pointers rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes
	 of the objects in otherArray remain unchanged.  If you know what you're doing this
	 may indeed be what you want.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."
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

