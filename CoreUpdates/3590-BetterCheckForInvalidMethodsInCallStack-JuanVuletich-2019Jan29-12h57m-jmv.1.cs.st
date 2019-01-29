'From Cuis 5.0 of 7 November 2016 [latest update: #3589] on 29 January 2019 at 1:03:47 pm'!
!classDefinition: #MethodInCallStackToBecomeInvalid category: #'Exceptions Kernel'!
Error subclass: #MethodInCallStackToBecomeInvalid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!MethodInCallStackToBecomeInvalid commentStamp: '<historical>' prior: 0!
A become operation tries to mutate an object that is the receiver ('self') in a method currently in execution, and part of the stack of calls of some process. This would render the method invalid and is potentially catastrophic.!


!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 1/29/2019 12:03:51'!
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
							signal: contextReceiver class name, ' has some instance running #', selector, ' that would become invalid.'.
						^ true ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!Object methodsFor: 'system primitives' stamp: 'jmv 1/29/2019 12:03:48'!
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
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.'.
						^ true ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 1/29/2019 12:03:50'!
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
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.'.
						^ true ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!Behavior methodsFor: 'private' stamp: 'jmv 8/16/2016 09:31:16'!
becomeCompact
	"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct index |

	Smalltalk isSpur ifTrue: [^ self halt: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self halt: 'You must not make a weak class compact'].
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name , 'is already compact'].
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

!Behavior methodsFor: 'private' stamp: 'jmv 11/27/2008 16:05'!
becomeUncompact
	| cct index |
	cct _ Smalltalk compactClassesArray.
	(index _ self indexIfCompact) = 0
		ifTrue: [^ self].
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


!ClassDescription methodsFor: 'initialization' stamp: 'jmv 1/29/2019 12:03:46'!
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
						signal: self class name, ' has some instance running ', oldMethod printString, ' that would become invalid.'.
					^nil ]]]
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


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/20/2015 12:51'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
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

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/20/2015 12:51'!
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
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

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

!ClassBuilder methodsFor: 'public' stamp: 'ar 7/19/1999 23:40'!
moveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName
	"Move the given instVar from srcClass to dstClass"
	(srcClass instVarNames includes: instVarName)
		ifFalse:[^self error: instVarName,' is not an instance variable of ', srcClass name].
	(prevInstVarName isNil or:[dstClass instVarNames includes: prevInstVarName])
		ifFalse:[^self error: prevInstVarName, 'is not an instance variable of', dstClass name].
	(srcClass inheritsFrom: dstClass) ifTrue:[
		"Move the instvar up the hierarchy."
		(self validateClass: srcClass forMoving: instVarName upTo: dstClass)
			ifFalse:[^false].
	].
	(dstClass inheritsFrom: srcClass) ifTrue:[
		"Move the instvar down the hierarchy"
		(self validateClass: srcClass forMoving: instVarName downTo: dstClass)
			ifFalse:[^false].
	].
	^self silentlyMoveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName! !

!methodRemoval: ProcessorScheduler #anyReceiverInStackIn:orIn:runningProcessSearchStart:!
ProcessorScheduler removeSelector: #anyReceiverInStackIn:orIn:runningProcessSearchStart:!
!methodRemoval: ProcessorScheduler #anyReceiverInStackIn:runningProcessSearchStart:!
ProcessorScheduler removeSelector: #anyReceiverInStackIn:runningProcessSearchStart:!
!methodRemoval: ProcessorScheduler #anyReceiverInStackIsKindOf:!
ProcessorScheduler removeSelector: #anyReceiverInStackIsKindOf:!
!classRemoval: #AttemptToMutateObjectInCallStack!
Smalltalk removeClassNamed: #AttemptToMutateObjectInCallStack!
