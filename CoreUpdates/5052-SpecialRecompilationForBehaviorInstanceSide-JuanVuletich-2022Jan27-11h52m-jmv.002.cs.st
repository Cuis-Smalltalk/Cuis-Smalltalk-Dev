'From Cuis 6.0 [latest update: #5049] on 27 January 2022 at 11:53:45 am'!

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/27/2022 11:36:54'!
recompileSingleRecursion: force from: oldClass to: newClass
	"Based on #recompile:from:to:mutate:
	Older behavior of that method (before January 2022) did a single BFS on the hierarchy tree,
	creating new classes when traveling downwards, and migrating instances before returning upwards.
	This was changed to build the entire hierarchy, then validate with #canUpdateFrom: and only keep
	the new classes, and migrate instances if #canUpdateFrom: answered true.
	This breaks when recompiling the instance side of the Behavior hierarchy (Behavior, ClassDescription,
	Class and Metaclass).
	This method, appropriate for these classes does a single recursion, i.e. the pre-2022 behavior.
	It uses a more restrictive check for existing instances: It will not allow recompilation if any instance is
	executing any method, regardless of it being affected by the reshaping of the class or not. For this
	reason, for other classes, we prefer the more careful check done by #canUpdateFrom:
	
	See senders."

	(newClass == oldClass and: [force not]) ifTrue:[
		^newClass].

	currentClassIndex _ 0.
	maxClassIndex _ oldClass withAllSubclasses size.

	(oldClass == newClass) ifTrue: [
		"Recompile from newClass without mutating"
		self informUserDuring: [
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll ]].
		^newClass].

	"Recompile oldClass to newClass"
	self informUserDuring: [
		self mutate: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'eem 6/11/2008 16:47'!
mutate: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Note: This method is slightly different from: #mutate:toSuper: since
	here we are at the root of reshaping and have two distinct roots."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self mutate: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self mutate: oldSubclass to: newSubclass.
		].
	].
	self update: oldClass to: newClass.
	^newClass! !


!ProcessorScheduler methodsFor: 'services' stamp: 'jmv 1/27/2022 11:08:03'!
anyReceiverInStackInstanceOfClassIn: anArray
	"Iterate over all methods currently in execution. Answer true if in any of them 'self class' an element of argument"
	"
	Processor anyReceiverInStackInstanceOfClassIn: { WorldMorph }
	Processor anyReceiverInStackInstanceOfClassIn: { Text }
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver class)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/27/2022 11:48:17'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory behaviorClasses |
	
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
	newCategory _ category withBlanksTrimmed.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	(oldClass notNil and: [oldClass includesBehavior: Behavior])
		ifTrue: [
			"This is only true for Behavior, ClassDescription, Class and Metaclass. Special treatment."
			unsafe ifFalse: [
				behaviorClasses _ (oldClass withAllSubclasses select: [ :c | c isMeta not ]) asArray.
				(Processor anyReceiverInStackInstanceOfClassIn: behaviorClasses) ifTrue: [
					self error: oldClass name,' has some subInstance running some CompiledMethod.'.
					^nil ]].
			"Please see comment at this method"
			newClass _ self recompileSingleRecursion: force from: oldClass to: newClass ]
		ifFalse: [
			"Other classes"
			newClass _ self recompile: force from: oldClass to: newClass mutate: false ].
	newClass isNil ifTrue: [ ^nil ].

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
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass! !


!ProcessorScheduler reorganize!
('accessing' activePriority activeProcess backgroundProcess highestPriority highestPriority: preemptedProcess processesDo: processesDo:withStackFramestDo:runningProcessSearchStart: processesWithTopContextDo:runningProcessSearchStart: waitingProcessesAt:)
('removing' remove:ifAbsent:)
('process state change' suspendFirstAt: suspendFirstAt:ifNone: terminateActive yield)
('priority names' highIOPriority lowIOPriority lowestPriority systemBackgroundPriority timingPriority userBackgroundPriority userInterruptPriority userSchedulingPriority)
('private' anyProcessesAbove:)
('objects from disk' objectForDataStream:)
('CPU usage tally' nextReadyProcess tallyCPUUsageFor:every:)
('printing' storeOn:)
('services' anyReceiverInStackIn: anyReceiverInStackInstanceOfClassIn:)
!

