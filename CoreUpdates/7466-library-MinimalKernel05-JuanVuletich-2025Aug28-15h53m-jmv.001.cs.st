'From Cuis7.5 [latest update: #7462] on 28 August 2025 at 3:55:22 pm'!

!ClassBuilder methodsFor: 'class definition' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 1/4/2024 15:18:32'!
newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
	"Create a new subclass of the given superclass with the given specification."
	| newFormat newClass |
	"Compute the format of the new class"
	newFormat := 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper.

	newFormat
		ifNil: [^ nil].

	(oldClass == nil or:[oldClass isMeta not]) 
		ifTrue:[newClass := self privateNewSubclassOf: newSuper from: oldClass]
		ifFalse:[newClass := oldClass shallowCopy].

	newClass 
		superclass: newSuper
		methodDictionary: MethodDictionary new
		format: newFormat;
		setInstVarNames: instVars.

	oldClass ifNotNil:[
		newClass organization: oldClass organization.
		"Recompile the new class"
		oldClass hasMethods 
			ifTrue:[newClass compileAllFrom: oldClass].
		self recordClass: oldClass replacedBy: newClass.
	].

	(oldClass == nil or:[oldClass isObsolete not]) 
		ifTrue:[newSuper addSubclass: newClass]
		ifFalse:[newSuper addObsoleteSubclass: newClass].

	^newClass! !

!ClassBuilder methodsFor: 'validation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 2/27/2025 10:18:23'!
checkInstSize: instSize errorMessage: errorMessage
	instSize > self class maxInstSize ifTrue:
		[self error: errorMessage.
		 ^false].
	instSize > self class maxAccessibleInstSize ifTrue:
		[Smalltalk isDevelopmentEnvironmentPresent ifTrue:
			[^self confirm: 'To access ', instSize printString, ' instance variables\requires the SistaV1 bytecode set.\See #preferredBytecodeSetEncoderClass:.' withNewLines].
		 ^false].
	^true! !

!ClassBuilder methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 2/27/2003 22:56'!
privateNewSubclassOf: newSuper
	"Create a new meta and non-meta subclass of newSuper"
	"WARNING: This method does not preserve the superclass/subclass invariant!!"
	| newSuperMeta newMeta |
	newSuperMeta := newSuper ifNil:[Class] ifNotNil:[newSuper class].
	newMeta := Metaclass new.
	newMeta 
		superclass: newSuperMeta 
		methodDictionary: MethodDictionary new 
		format: newSuperMeta format.
	^newMeta new
! !

!ClassBuilder methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 1/4/2024 15:17:05'!
privateNewSubclassOf: newSuper from: oldClass
	"Create a new meta and non-meta subclass of newSuper using oldClass as template"
	"WARNING: This method does not preserve the superclass/subclass invariant!!"
	| newSuperMeta oldMeta newMeta |
	oldClass ifNil:[^self privateNewSubclassOf: newSuper].
	newSuperMeta := newSuper ifNil:[Class] ifNotNil:[newSuper class].
	oldMeta := oldClass class.
	newMeta := oldMeta shallowCopy.
	newMeta 
		superclass: newSuperMeta
		methodDictionary: MethodDictionary new
		format: (self computeFormat: oldMeta typeOfClass 
					instSize: oldMeta instVarNames size 
					forSuper: newSuperMeta);
		setInstVarNames: oldMeta instVarNames;
		organization: oldMeta organization.
	"Recompile the meta class"
	oldMeta hasMethods 
		ifTrue:[newMeta compileAllFrom: oldMeta].
	"Record the meta class change"
	self recordClass: oldMeta replacedBy: newMeta.
	"And create a new instance"
	^newMeta adoptInstance: oldClass from: oldMeta! !

!ClassBuilder methodsFor: 'class format' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 2/27/2025 10:23:27'!
computeFormat: type fullInstancesSize: newInstSize
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	| isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue: [
		newInstSize > 0 ifTrue: [
			self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	(self checkInstSize: newInstSize
		 errorMessage:  'Class has too many instance variables (', newInstSize printString,')') ifFalse: [
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isPointers := true].
	type == #bytes ifTrue:[isVar := true. bitsUnitSize := 1. isPointers := isWeak := false].
	type == #shorts ifTrue:[isVar := true. bitsUnitSize := 2. isPointers := isWeak := false].
	type == #words ifTrue:[isVar := true. bitsUnitSize := 4. isPointers := isWeak := false].
	type == #longs ifTrue:[isVar := true. bitsUnitSize := 8. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isPointers := true].
	type == #ephemeron ifTrue:[isVar := false. isWeak := isPointers := true].
	type == #immediate ifTrue:[isVar := isWeak := isPointers := false].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and: [newInstSize > 0]) ifTrue: [
		self error: 'A non-pointer class cannot have named instance variables'.
		^nil].
	^self format: newInstSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak! !

!ClassBuilder methodsFor: 'class format' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 1/4/2024 15:17:52'!
computeFormat: type instSize: newInstSize forSuper: newSuper
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	^ self computeFormat: type fullInstancesSize: (newInstSize + (newSuper ifNil: [0] ifNotNil: [newSuper instSize]))! !

!ClassBuilder methodsFor: 'class format' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 1/4/2024 16:13:22'!
format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak

	"Compute the format for the given instance specfication.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character, SmallFloat64)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap, WideString)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable (ByteString)
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := isWeak
					ifTrue:
						[isVar
							ifTrue: [4]
							ifFalse: [5]]
					ifFalse:
						[isPointers
							ifTrue:
								[isVar
									ifTrue: [nInstVars > 0 ifTrue: [3] ifFalse: [2]]
									ifFalse: [nInstVars > 0 ifTrue: [1] ifFalse: [0]]]
							ifFalse:
								[isVar
									ifTrue: [bitsUnitSize caseOf: {
											[1] -> [16].
											[2] -> [12].
											[4] -> [10].
											[8] -> [9] }]
									ifFalse: [7]]].
	^(instSpec bitShift: 16) + nInstVars! !

!ClassBuilder methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 11/27/2008 16:00'!
initialize
	instVarMap := IdentityDictionary new.! !


!ClassBuilder reorganize!
('class definition' class:instanceVariableNames:unsafe: doneCompiling: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:unsafe: needsSubclassOf:type:instanceVariables:from: newSubclassOf:type:instanceVariables:from: recompile:from:to:mutate: recompileSingleRecursion:from:to:)
('validation' checkInstSize:errorMessage: validateClassName: validateClassvars:from:forSuper: validateInstvars:from:forSuper: validateSubclass:canKeepLayoutFrom:forSubclassFormat: validateSubclassFormat:from:forSuper:extra: validateSuperclass:forSubclass:)
('private' informUserDuring: privateNewSubclassOf: privateNewSubclassOf:from: recordClass:replacedBy: reservedNames showProgressFor: tooDangerousClasses)
('public' class:instanceVariableNames: superclass:ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('class mutation' createHierarchy:to: mutate:to: mutateInstances:to: reshapeClass:toSuper: update:to:)
('class format' computeFormat:fullInstancesSize: computeFormat:instSize:forSuper: format:variable:bitsUnitSize:pointers:weak:)
('initialization' initialize)
!

