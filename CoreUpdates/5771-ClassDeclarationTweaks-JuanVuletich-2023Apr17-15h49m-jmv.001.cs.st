'From Cuis 6.0 [latest update: #5769] on 17 April 2023 at 3:50:20 pm'!

!Class methodsFor: 'initialization' stamp: 'jmv 4/17/2023 15:32:28'!
declareClassVarString: aString
	"Declare class variables common to all instances.
	Answer whether recompilation is advisable."

	| varNames |
	varNames := (Scanner new scanFieldNames: aString) collect: [ :x | x asSymbol ].
	varNames do: [ :varName |
		varName first isLowercase ifTrue: [
			self error: varName, ' class variable name should be capitalized; proceed to include anyway.' ]].
	^self declareClassVariables: varNames.! !

!Class methodsFor: 'initialization' stamp: 'jmv 4/17/2023 15:26:52'!
declareClassVariables: newVars
	"Declare class variables common to all instances.
	Answer whether recompilation is advisable."

	| conflicts |
	conflicts := false.
	classPool
		ifNotNil: [
			(classPool keys reject: [ :x | newVars includes: x ]) 
				do: [ :var | self removeClassVarName: var ]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [ :var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var , ' is defined elsewhere'.
					conflicts := true ]].
	newVars size > 0
		ifTrue: [
			classPool := self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared ]].
	^ conflicts.! !

!Class methodsFor: 'initialization' stamp: 'jmv 4/17/2023 15:38:34'!
declareSharedPoolString: poolString
	"Set up sharedPools. Answer whether recompilation is advisable."

	^self declareSharedPools: (Scanner new scanFieldNames: poolString)! !

!Class methodsFor: 'initialization' stamp: 'jmv 4/17/2023 15:38:00'!
declareSharedPools: sharedPoolNames
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools := self sharedPools.
	sharedPools := OrderedCollection new.
	sharedPoolNames do: [ :poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent: [
			(PoolDefinitionNotification signalNamed: poolName)
				ifTrue: [ Smalltalk at: poolName asSymbol put: Dictionary new ]
				ifFalse: [^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [ sharedPools := nil ].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]].! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 4/17/2023 15:38:54'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory behaviorClasses |
	
	instVars := Scanner new scanFieldNames: instVarString.
	classVars := (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass := Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass := nil]. "Already checked in #validateClassName:"
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
		"Create the new class"
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
	newCategory := category withBlanksTrimmed.
	organization := Smalltalk organization.
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
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass! !

!methodRemoval: Class #declare: stamp: 'jmv 4/17/2023 15:49:40'!
Class removeSelector: #declare:!
!methodRemoval: Class #sharing: stamp: 'jmv 4/17/2023 15:49:40'!
Class removeSelector: #sharing:!
