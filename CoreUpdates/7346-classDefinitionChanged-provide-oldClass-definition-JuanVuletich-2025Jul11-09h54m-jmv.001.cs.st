'From Cuis7.5 [latest update: #7345] on 11 July 2025 at 9:55:23 am'!

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 7/11/2025 09:29:27'!
classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass . oldClassDefinition }! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/11/2025 09:43:21'!
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

	"Create the new class"
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

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 7/11/2025 09:43:14'!
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

!ClassBuilder methodsFor: 'private' stamp: 'jmv 7/11/2025 09:43:04'!
recordClass: oldClass replacedBy: newClass
	"Keep the changes up to date when we're moving instVars around"

	(instVarMap includesKey: oldClass name) ifTrue:[
		SystemChangeNotifier uniqueInstance
			classDefinitionChangedFrom: oldClass
			to: newClass
			oldClassDefinition: oldClass definition.
	].! !


!Class methodsFor: 'class variables' stamp: 'jmv 7/11/2025 09:42:53'!
addClassVarName: aString
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	| symbol oldState oldDefinition |
	oldDefinition := self definition.
	oldState := self copy.
	aString first isLowercase
		ifTrue: [^self error: aString, ' class variable name should be capitalized; proceed to include anyway.'].
	symbol := aString asSymbol.
	self withAllSubclasses do: 
		[:subclass | 
		(subclass bindingOf: symbol) ifNotNil:[
			^ self error: aString 
				, ' is already used as a variable name in class ' 
				, subclass name]].
	classPool ifNil: [classPool := Dictionary new].
	(classPool includesKey: symbol) ifFalse: [
		"Pick up any refs in Undeclared"
		classPool declare: symbol from: Undeclared.
		SystemChangeNotifier uniqueInstance
			classDefinitionChangedFrom: oldState
			to: self
			oldClassDefinition: oldDefinition ].! !

!methodRemoval: SystemChangeNotifier #classDefinitionChangedFrom:to: stamp: 'jmv 7/11/2025 09:54:57'!
SystemChangeNotifier removeSelector: #classDefinitionChangedFrom:to:!
