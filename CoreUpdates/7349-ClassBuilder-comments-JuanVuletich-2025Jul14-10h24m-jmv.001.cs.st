'From Cuis7.5 [latest update: #7349] on 14 July 2025 at 10:55:42 am'!

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/14/2025 10:40:17'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass oldClassDefinition |
	instVars := Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew := self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class. Include all methods from oldClass recompiled for it."
	oldClassDefinition := oldClass definition.
	copyOfOldClass := oldClass copy.
	newClass := self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.

	"Please see comment at this method. Also see other senders."
	newClass := self recompileSingleRecursion: false from: oldClass to: newClass.
	newClass isNil ifTrue: [ ^nil ].

	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance
		classDefinitionChangedFrom: copyOfOldClass to: newClass oldClassDefinition: oldClassDefinition.
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/14/2025 10:39:06'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory behaviorClasses oldClassDefinition |
	
	instVars := Scanner new scanFieldNames: instVarString.
	classVars := (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass := Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass := nil]. "Already checked in #validateClassName:"
	oldClassDefinition := oldClass ifNotNil: [ oldClass definition ].
	copyOfOldClass := oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew := self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
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
		"Create the new class. Include all methods from oldClass recompiled for it."
		newClass := self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass := oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force := (newClass declareClassVarString: classVarString) | (newClass declareSharedPoolString: poolString).

	"... classify ..."
	organization := Smalltalk organization.
	newCategory := (category isNil or: [ category isEmpty ])
		ifFalse: [category withBlanksTrimmed]
		ifTrue: [organization class defaultClassCategory].
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	(oldClass notNil and: [oldClass includesBehavior: Behavior])
		ifTrue: [
			"This is only true for Behavior, ClassDescription, Class and Metaclass. Special treatment."
			unsafe ifFalse: [
				behaviorClasses := (oldClass withAllSubclasses select: [ :c | c isMeta not ]) asArray.
				(Processor anyReceiverInStackInstanceOfClassIn: behaviorClasses) ifTrue: [
					self error: oldClass name,' has some subInstance running some CompiledMethod.'.
					^nil ]].
			"Please see comment at this method"
			newClass := self recompileSingleRecursion: force from: oldClass to: newClass ]
		ifFalse: [
			"Other classes"
			newClass := self recompile: force from: oldClass to: newClass mutate: false ].
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
	SystemChangeNotifier uniqueInstance
		classDefinitionChangedFrom: copyOfOldClass to: newClass oldClassDefinition: oldClassDefinition.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/14/2025 10:53:40'!
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

	"Recompile oldClass and subclasses to newClass and subclasses.
	Recompile all the methods in the newly created classes."
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

	"Make newClass thake the place of oldClass in the system: update instances and references.
	Do it for the whole hierarchy."
	self informUserDuring: [
		self mutateInstances: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/14/2025 10:43:45'!
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

	currentClassIndex := 0.
	maxClassIndex := oldClass withAllSubclasses size.

	(oldClass == newClass) ifTrue: [
		"Recompile from newClass without mutating"
		self informUserDuring: [
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll ]].
		^newClass].

	"Recompile oldClass and subclasses to newClass and subclasses.
	Recompile all the methods in the newly created classes."
	self informUserDuring: [
		self mutate: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 7/14/2025 10:51:45'!
createHierarchy: oldClass to: newClass
	"Create a new class hierarchy from the old class and subclasses into newClass and subclasses.
	Recompile all the methods in the newly created classes."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		"create new class. recompile all methods for it."
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self createHierarchy: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			"create new class. recompile all methods for it."
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self createHierarchy: oldSubclass to: newSubclass.
		].
	].
	^newClass! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'eem 6/11/2008 16:47'!
mutate: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Recompile all the methods in the newly created classes."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		"create new class. recompile all methods for it."
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self mutate: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			"create new class. recompile all methods for it."
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self mutate: oldSubclass to: newSubclass.
		].
	].
	"New class creation with all new methods is done.
	Make newClass thake the place of oldClass in the system: update instances and references."
	self update: oldClass to: newClass.
	^newClass! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 7/14/2025 10:54:49'!
mutateInstances: oldClass to: newClass
	"Make newClass thake the place of oldClass in the system: update instances and references.
	Do this for the whole sub hierarchy."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do: [ :oldSubclass | | newSubclass |
		newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ]. 
		self mutateInstances: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do: [ :oldSubclass | | newSubclass |
		oldSubclass ifNotNil: [
			newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ] ifNone: nil.
			newSubclass ifNotNil: [
				self mutateInstances: oldSubclass to: newSubclass ].
		].
	].
	"Make newClass thake the place of oldClass in the system: update instances and references."
	self update: oldClass to: newClass.
	^newClass! !

