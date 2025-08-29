'From Cuis7.5 [latest update: #7456] on 28 August 2025 at 3:04:20 pm'!

!Behavior methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 12/31/2009 09:48'!
binding
	^ nil -> self! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 5/17/2003 14:06'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	^superclass bindingOf: varName! !

!Behavior methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 5/18/2003 18:13'!
classBindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver's class"
	^self bindingOf: varName! !

!Behavior methodsFor: 'copying' library: #MinimalKernel05ClassAndMethodCreation stamp: 'SqR 2/20/2015 21:09'!
postCopy

	self methodDict: self methodDict copy! !

!Behavior methodsFor: 'testing class hierarchy' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 3/12/98 12:36'!
includesBehavior: aClass
	^self == aClass or:[self inheritsFrom: aClass]! !

!Behavior methodsFor: 'testing class hierarchy' library: #MinimalKernel05ClassAndMethodCreation stamp: 'Squeak1.0 9/20/1996 10:50:05'!
inheritsFrom: aClass 
	"Answer whether the argument, aClass, is on the receiver's superclass 
	chain."

	| aSuperclass |
	aSuperclass := superclass.
	[aSuperclass == nil]
		whileFalse: 
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass := aSuperclass superclass].
	^false! !

!Behavior methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 5/31/1983 9:10:35'!
flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has 
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed! !

!Behavior methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 7/15/1999 16:39'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver.
	Must only be sent to a new instance; else we would need Object flushCache."
	superclass := aClass.
	format := fmt.
	methodDict := mDict.! !

!Behavior methodsFor: 'instances and variables' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 5/13/2025 09:26:11'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables.
	 This depends on the bytecode set.
	Note: On images without Compiler / Parser / Encoder we'd need a class variable somewhere.
		Assume EncoderForV3PlusClosures if unknown"

	CompiledMethod preferredBytecodeSetEncoderClass isNil
		ifTrue: [ ^256 ].
	^CompiledMethod preferredBytecodeSetEncoderClass maxAccessibleInstSize! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 2/28/2010 22:47'!
addSelectorSilently: selector withMethod: compiledMethod 
	"Add the message selector with the corresponding compiled method to the 
	receiver's method dictionary.
	Do this without sending system change notifications"

	| oldMethodOrNil |
	oldMethodOrNil := self lookupSelector: selector.
	self methodDict at: selector put: compiledMethod.

	"Now flush Squeak's method cache, either by selector or by method"
	oldMethodOrNil ifNotNil: [oldMethodOrNil flushCache].
	selector flushCache.! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'di 2/17/2000 22:37'!
compiledMethodAt: selector 
	"Answer the compiled method associated with the argument, selector (a 
	Symbol), a message selector in the receiver's method dictionary. If the 
	selector is not in the dictionary, create an error notification."

	^ self methodDict at: selector! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'di 3/27/1999 23:20'!
includesSelector: aSymbol 
	"Answer whether the message whose selector is the argument is in the 
	method dictionary of the receiver's class."

	^ self methodDict includesKey: aSymbol! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'di 1/2/1999 15:45'!
lookupSelector: selector
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Return nil if no method is found."
	| lookupClass |
	lookupClass := self.
	[lookupClass == nil]
		whileFalse: 
			[(lookupClass includesSelector: selector)
				ifTrue: [^ lookupClass compiledMethodAt: selector].
			lookupClass := lookupClass superclass].
	^ nil! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 2/28/2010 22:50'!
methodDict
	methodDict ifNil: [self recoverFromMDFaultWithTrace].
	^ methodDict! !

!Behavior methodsFor: 'method dictionary' library: #MinimalKernel05ClassAndMethodCreation stamp: 'dvf 9/27/2005 17:08'!
methodDict: aDictionary
	methodDict := aDictionary! !


!ClassDescription methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'HAW 3/12/2019 22:09:16'!
localBindingOf: varNameSymbol

	self subclassResponsibility ! !

!ClassDescription methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'NS 4/6/2004 15:26'!
organization: aClassOrg
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	organization := aClassOrg! !

!ClassDescription methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 5/12/2025 12:01:54'!
setInstVarNames: instVarArray
	"Private - for class initialization only"
	| required |
	required := self instSize.
	superclass ifNotNil: [
		required := required - superclass instSize].
	instVarArray size = required
		ifFalse:[
			^self error: required printString, ' instvar names are required'].
	instanceVariables := instVarArray size = 0
		ifFalse: [
			instVarArray == Array
				ifTrue: [ instVarArray ]
				ifFalse: [ Array withAll: instVarArray ]].! !

!ClassDescription methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'NS 4/6/2004 15:31'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	instanceVariables := nil.
	self organization: nil.! !


!Class methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 12/31/2009 09:49'!
binding

	^ Smalltalk associationAt: name ifAbsent: [nil -> self]! !

!Class methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 5/24/2014 11:00'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol |
	aSymbol := varName asSymbol.

	"First look in receiver, and up the hierarchy. Inherited variables take precedence over globals."
	(self localBindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in globals."
	(Smalltalk bindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Fail at the end."
	^nil! !

!Class methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'HAW 10/5/2020 08:09:51'!
classVarBindingOf: varNameSymbol

	^self classPool bindingOf: varNameSymbol! !

!Class methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'HAW 10/5/2020 08:08:59'!
localBindingOf: varNameSymbol
	"Answer the binding of some variable resolved in the scope of the receiver."

	"First look in classVar dictionary."
	(self classVarBindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		(pool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].
	].

	"Finally look higher up the superclass chain and fail at the end."
	^superclass ifNotNil: [ superclass localBindingOf: varNameSymbol ]! !

!Class methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 5/12/2025 17:16:17'!
addSubclass: aSubclass
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."

	aSubclass superclass ~~ self
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		subclasses := Array with: aSubclass.
		^self].
	subclasses do: [ :cl | cl == aSubclass ifTrue: [ ^self ]]. "Already my subclass"
	subclasses := Array withAll: subclasses with: aSubclass.! !

!Class methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 9/14/2023 10:10:53'!
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
					self error: var, ' is defined elsewhere'.
					conflicts := true ]].
	newVars size > 0
		ifTrue: [
			"in case it was nil"
			classPool := self classPool.
			newVars do: [:var |
				classPool declare: var from: 
					(Smalltalk at: #Undeclared ifAbsent: nil) ]].
	^ conflicts.! !

!Class methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 7/15/1999 15:37'!
setName: aSymbol
	"Private - set the name of the class"
	name := aSymbol.! !

!Class methodsFor: 'initialization' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 7/20/1999 11:23'!
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	subclasses := nil. "Important for moving down the subclasses field into Class"
! !

!Class methodsFor: 'copying' library: #MinimalKernel05ClassAndMethodCreation stamp: 'nice 12/29/2010 10:47'!
copy 
	"Answer a copy of the receiver without a list of subclasses."
	| newClass |
	newClass := self class copy new
		superclass: superclass
		methodDict: self methodDict copy
		format: format
		name: name
		organization: self organization copy
		instVarNames: instanceVariables copy
		classPool: classPool copy
		sharedPools: sharedPools copy.
	Class instSize+1 to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	^ newClass! !


!Metaclass methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ar 5/18/2003 18:13'!
bindingOf: varName

	^thisClass classBindingOf: varName! !

!Metaclass methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'HAW 3/12/2019 22:08:40'!
localBindingOf: varNameSymbol

	^thisClass localBindingOf: varNameSymbol ! !

!Metaclass methodsFor: 'copying' library: #MinimalKernel05ClassAndMethodCreation stamp: 'nice 12/29/2010 10:21'!
postCopy
	"Don't share the reference to the sole instance."

	super postCopy.
	thisClass := nil.! !

!Metaclass methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'nk 11/9/2003 10:00'!
new
	"The receiver can only have one instance. Create it or complain that
	one already exists."

	thisClass class ~~ self
		ifTrue: [^thisClass := self basicNew]
		ifFalse: [self error: 'A Metaclass should only have one instance!!']! !


!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 5/11/2012 15:40'!
endPC
	"Answer the index of the last bytecode."
	| size flagByte |
	"Can't create a zero-sized CompiledMethod so no need to use last for the errorEmptyCollection check.
	 We can reuse size."
	size := self size.
	flagByte := self at: size.
	flagByte = 0 ifTrue: [
		"If last byte = 0, may be either 0, 0, 0, 0 or just 0"
		size-1 to: size-3 by: -1 do: [ :i |
			i < self initialPC ifTrue: [ ^ i ].
			(self at: i) = 0 ifFalse: [ ^ i ]].
		^size - 4].
	flagByte < 252 ifTrue: [
		"Magic sources (temp names encoded in last few bytes)"
		^flagByte <= 127
			ifTrue: [size - flagByte - 1]
			ifFalse: [size - (flagByte - 128 * 128) - (self at: size - 1) - 2]].
	"Normal 4-byte source pointer"
	^size - 4! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 12/17/2016 22:38:52'!
initialPC
	"Answer the program counter for the receiver's first bytecode."
	^ (self numLiterals + 1) * Smalltalk wordSize + 1! !

!CompiledMethod methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'md 2/16/2006 11:30'!
methodClass: aClass
	"set the class binding in the last literal to aClass"
	self literalAt: self numLiterals put: aClass binding! !

!CompiledMethod methodsFor: 'literals' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 5/31/1983 9:10:35'!
literalAt: index put: value 
	"Replace the literal indexed by the first argument with the
	second argument."

	^self objectAt: index + 1 put: value! !

!CompiledMethod methodsFor: 'literals' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 5/31/1983 9:10:35'!
objectAt: index put: value 
	"Store the value argument into a literal in the receiver.   An index of 2 
	corresponds to the first literal.  Fails if the index is less than 2 or greater than 
	the number of literals.  Answer the value as the result.  Normally only the
	compiler sends this message, because only the compiler stores values in 
	CompiledMethods.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed! !

!CompiledMethod methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 8/30/2023 12:04:05'!
flushCache
	"Tell the virtual machine to remove all references to this method from its method
	 lookup cache(s), if it has them. This is provided for backwards compatibility.
	 When a method is redefined (added to, removed from, or replaced in, a method
	 dictionary) then Symbol flushCache should be used. This is because adding or
	 removing a method may alter the method that a given selector should invoke
	 for methods other than the receiver.  For example, if a subclass inherits a
	 method and this is overridden, flushing the new method will not flush the existing
	 method.

	 If a method is modified in-place (for example, some bytecode is replaced by
	 bytecode that effects a breakpoint) then voidCogVMState should be used."

	<primitive: 116>! !

!CompiledMethod methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 11/29/2008 11:52'!
penultimateLiteral: anObject
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex put: anObject]
		ifFalse: [self error: 'insufficient literals']! !

!CompiledMethod methodsFor: 'private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 8/30/2023 12:17:03'!
voidCogVMState
	"Tell the VM to remove all references to any machine code form of the method.
	 This primitive must be called whenever a method is in use and modified.  This is
	 more aggressive (and *much* more costly) than flushCache since it must search
	 through all context objects, making sure that none have a (hidden) machine code pc
	 in the receiver.  Since modifying a method will likely change the generated machine code,
	 modifying a method (rather than redefining it) requires this more aggressive flush.

	If this is needed for a bunch of methods, and not just one, consider calling
	Smalltalk voidCogVMState
	(primitive 214)"

	<primitive: 215>
	^self flushCache! !


!CompiledMethod class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'tk 9/9/2000 20:36'!
basicNew: size

	self error: 'CompiledMethods may only be created with newMethod:header:' ! !

!CompiledMethod class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 7/31/2016 16:11:49'!
createMethod: numberOfBytesForAllButTrailer trailer: bytes header: headerWord 
	| meth delta |
	meth := self newMethod: numberOfBytesForAllButTrailer + bytes size header: headerWord.
	"copy the encoded trailer data"
	delta := meth size - bytes size.
	1 to: bytes size do: [ :i |
		meth at: delta + i put: (bytes at: i)].
	^meth! !

!CompiledMethod class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'Squeak1.2 6/29/1997 10:40:55'!
new
	"This will not make a meaningful method, but it could be used
	to invoke some otherwise useful method in this class."
	^ self newMethod: 0 header: 0! !

!CompiledMethod class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'tk 1/21/2000 15:25'!
new: size

	self error: 'CompiledMethods may only be created with newMethod:header:'! !

!CompiledMethod class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 8/13/2014 05:55'!
newMethod: numberOfBytes header: headerWord
	"Primitive. Answer an instance of me. The number of literals (and other 
	 information) is specified by the headerWord (see my class comment).
	 The first argument specifies the number of fields for bytecodes in the
	 method. Fail if either argument is not a SmallInteger, or if numberOfBytes
	 is negative, or if memory is low. Once the header of a method is set by
	 this primitive, it cannot be changed to change the number of literals.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingNewMethod: numberOfBytes header: headerWord].
	^self primitiveFailed! !

!CompiledMethod class methodsFor: 'accessing' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 12/26/2024 13:45:51'!
preferredBytecodeSetEncoderClass
	^PreferredBytecodeSetEncoderClass! !


!SystemDictionary methodsFor: 'miscelaneous' library: #MinimalKernel05ClassAndMethodCreation stamp: 'eem 8/28/2009 14:52'!
voidCogVMState
	"Void any internal caches the VM maintains other than the method lookup caches.
		 These comprise
				- the stack zone, where method activations are stored, and
				- the machine code zone, where the machine code form of CompiledMethods is held."
	<primitive: 214>
	^self primitiveFailed

	"Time millisecondsToRun: [Smalltalk voidCogVMState]"
	"(1 to: 10) collect: [:ign| Time millisecondsToRun: [Smalltalk voidCogVMState]]"! !


!MethodDictionary class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 2/5/2024 18:26:44'!
new
	"change the default size to be a bit bigger to help reduce the number of #grows while filing in"
	^self newWithRoomForMoreThan: 16! !

!MethodDictionary class methodsFor: 'instance creation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'di 11/4/97 20:11'!
newWithRoomForMoreThan: nElements
	"Create a Dictionary large enough to hold nElements without growing.
	Note that the basic size must be a power of 2.
	It is VITAL (see grow) that size gets doubled if nElements is a power of 2"
	| size |
	size := 1 bitShift: nElements highBit.
	^ (self basicNew: size) init: size! !


!Behavior reorganize!
('[mkClassAndMethodCreation] accessing')
('[mkClassAndMethodCreation] copying')
('[mkClassAndMethodCreation] initialization')
('[mkClassAndMethodCreation] instances and variables')
('[mkClassAndMethodCreation] method dictionary')
('[mkClassAndMethodCreation] private')
('[mkClassAndMethodCreation] testing class hierarchy')
('accessing' binding bindingOf: classBindingOf: classDepth decompilerClass definesInstanceVariableNamed: format instSize instSpec instVarNames name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('auto complete' typeName)
('class hierarchy' deepSubclassNamed: superclass)
('comparing' identityHash)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges variablesAndOffsetsDo:)
('constants' maxInstSize)
('copying' copyOfMethodDictionary postCopy)
('creating method dictionary' methodDictionary:)
('enumerating' allInstancesDo: allSubInstancesDo: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('instances' allInstancesOrNil someInstance)
('methods' addMethodsTo:thatReferExplicitlyTo: addMethodsTo:thatReferTo: addTo:methodsThatReferInHierarchyTo: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: stampAt:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteClassesCollection removeObsoleteSubclass:)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('release' obsolete)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('system-support' allCallsOn)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('initialization' superclass:methodDictionary:format:)
('instances and variables' maxAccessibleInstSize)
('method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict methodDict:)
!


!ClassDescription reorganize!
('[mkClassAndMethodCreation] accessing')
('[mkClassAndMethodCreation] initialization')
('accessing' classVersion comment comment: comment:stamp: hasComment instVarNames localBindingOf: smalltalkComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inCategory:methodTimeStamp:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:libraryName:notifying: compile:classified:withStamp:libraryName:notifying:logSource: compile:classified:withStamp:notifying: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:library:stamp: methodsFor:library:stamp:prior: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on:withStamp:libraryName:priorMethod: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling organization: setInstVarNames: superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying: newInstanceFrom:variable:size:map:)
!


!Class reorganize!
('[mkClassAndMethodCreation] accessing')
('[mkClassAndMethodCreation] copying')
('[mkClassAndMethodCreation] initialization')
('accessing' binding bindingOf: classVarBindingOf: localBindingOf: name)
('accessing class hierarchy' hasSubclasses removeSubclass: subclasses subclassesDo: superclass:)
('class name' rename: safeRenameTo:)
('class variables' addClassVarName: allClassVarNames classPool classPool: classPoolFrom: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('compiling' compileAllFrom: possibleVariablesFor:continuedFrom:)
('enumerating' allSubclassesSortedDo: subclassesSortedDo:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutAlphabetically fileOutHierarchy fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('initialization' addSubclass: declareClassVarString: declareClassVariables: declareSharedPoolString: declareSharedPools: setName: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('printing' printNameOn:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('subclass creation' ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('subclass creation-convenience' subclass:instanceVariableNames:classVariableNames:poolDictionaries:)
('testing' hasMethods isObsolete)
('private' spaceUsed)
('SUnit tests' testCaseClass)
('copying' copy)
!


!Metaclass reorganize!
('[mkClassAndMethodCreation] accessing')
('[mkClassAndMethodCreation] copying')
('[mkClassAndMethodCreation] instance creation')
('accessing' allClassInstVarNames allInstances bindingOf: category localBindingOf: name soleInstance theMetaClass theNonMetaClass)
('accessing variables' allClassVarNames)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo:)
('class variables' classPool)
('compiling' acceptsLoggingOfCompilation possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('enumerating' allInstancesDo:)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('initialization' adoptInstance:from: instanceVariableNames:)
('instance variables' addInstVarName: removeInstVarName:)
('printing' printNameOn:)
('testing' canZapMethodDictionary isMeta isObsolete)
('SUnit tests' testCaseClass)
('copying' postCopy)
('instance creation' new)
!


!CompiledMethod reorganize!
('[mkObjectSerialization] object serialization' readDataFrom:size:)
('[mkClassAndMethodCreation] accessing')
('[mkClassAndMethodCreation] literals')
('[mkClassAndMethodCreation] private')
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass endPC flag frameSize initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: setSignFlag signFlag trailer withPropertiesDo:ifSelector:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: storeDataOn: zapSourcePointer)
('initialization' copyWithBytecodes: copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: header headerDescription implicitLiteralsDo: indexOfLiteral: literalAt: literalAt:put: literalStrings literals literalsDo: objectAt: objectAt:put: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasLiteralExplicitly: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper usesClosureBytecodes writesInstanceVariable:)
('time stamp' author dateAndTime dateMethodLastSaved timeStamp)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' flushCache penultimateLiteral penultimateLiteral: voidCogVMState)
!


!CompiledMethod class reorganize!
('[mkClassAndMethodCreation] instance creation')
('[mkClassAndMethodCreation] accessing')
('class initialization' fullFrameSize initialize smallFrameSize)
('instance creation' basicNew: createMethod:trailer:header: new new: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: newFrom: newInstanceFrom:variable:size:map: newMethod:header: toReturnConstant:trailerBytes: toReturnField:trailerBytes: toReturnSelf toReturnSelfTrailerBytes:)
('services' timeStamp:partsDo: unboundMethods)
('evaluating' receiver:withArguments:executeMethod:)
('method encoding' byteCodeSetsKnownToTheVM headerFlagForEncoderClass:)
('private' handleFailingFailingNewMethod:header: handleFailingNewMethod:header:)
('constants' maxNumLiterals)
('source code management aux' classCategoryFrom: field:from:ifAbsentOrNil: libraryNameFrom: priorReferenceFrom: stampFrom:)
('preferences' preferredBytecodeSetEncoderClass: useSistaBytecodes useV3Bytecodes)
('accessing' preferredBytecodeSetEncoderClass)
!


!SystemDictionary reorganize!
('[mkHashedCollections] globals' at:put: flushClassNameCache)
('[mkHashedCollections] system attributes' maxIdentityHash)
('[mkVMnImage] primitives' primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primImageName primVmFileName primVmPath)
('[mkVMnImage] miscelaneous' cachedSystemAttribute: getSystemAttribute: listBuiltinModule: listLoadedModule: platformName primGetSystemAttribute:)
('[mkVMnImage] special objects' specialObjectsArray)
('[mkClassAndMethodCreation] miscelaneous')
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameClassNamed:as: renamedClass:from:)
('globals' associationOrUndeclaredAt: declare:)
('housekeeping' browseObsoleteMethodReferences browseUndeclaredReferences cleanOutUndeclared condenseChanges condenseSources condenseSources: macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 undeclaredReferences verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imagePath primImageName: userChangesFileName vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' canBeInterrupted: cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModules listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformSubtype unloadModule: vmFileName vmOptionsDo: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allImplementorsOf:subTo: allImplementorsOf:superTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithSourceString:matchCase:scopeClass:includeSubclasses:includeSuperclasses: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSelect:localTo: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers rootClasses unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: askConfirmationOnQuit filenameForUpdatedImage: lastQuitLogPosition logSnapshot:andQuit: nopTag printStuffToCleanOnImageSave processShutDownList: processStartUpList: quit quitNoSaveTag quitPrimitive: quitTag readAndApplyUserPrefs removeFromShutDownList: removeFromStartUpList: saveAndQuit saveAndQuitAs:clearAllClassState: saveAndStayAs:clearAllClassState: saveAs saveAs: saveAs:clearAllClassState: saveImage saveRollingReleaseAndQuit saveStableRelease send:toClassesNamedIn:with: setDefaultGCParameters setGCParameters setPlatformPreferences snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: logToUserChanges:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: versionString)
('special objects' clearExternalObjects externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: confirmRemovalOf:on:andSubclasses: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing - main' browse browseFull: browseFull:selector: browseHierarchy: browseHierarchy:selector: browserWindowClass findAndBrowseClass findInSourceCode)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllClasses browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOf:subTo: browseAllImplementorsOf:superTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:localTo: browseAllSelect:name:autoHighlight: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqEqSentToCharacterConstants browseEqEqSentToNumericConstants browseEqEqSentToSmallIntegerConstants browseEqSentToCharacterConstants browseEqSentToNumericConstants browseEqSentToSmallIntegerConstants browseInstVarRefs: browseLikelyUnneededRedefinitions browseMessageList:name: browseMessageList:name:autoHighlight:allOccurrences: browseMessageListUnsorted:name:autoHighlight: browseMethodsWithSourceString: browseMethodsWithSourceString:in: browseMethodsWithSourceString:scopeClass:includeSubclasses:includeSuperclasses: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseSendersOf:notTo: browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive)
('Closure measurements' browseMethodsWithEmptyClosures)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields: processSuspensionUnblocks)
('query' classesWithExtensionCategories hierarchySorted:do: methodsWithUnboundGlobals)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' checkIfAlreadyRunningOrStoppedNoExit displayCommandLineUsageOn: displaySystemInformationStringOn: doStartUp: lastCommandLineArguments logStartupDebugAid: logStartupDebugAids logStartupError: logStartupNotice: processCommandLineArguments: processFinalCommandLineOption:optionArgument:raiseExceptions: processInitialCommandLineOption:optionArgument: readCommandLineArguments setStartupStamp startUpArguments startUpScriptAllArguments startUpScriptArguments)
('startup - recover user changes' warnAndOfferLastUserChanges)
('startup - restore lost changes' isQuitNoSaveRecord: isQuitRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('vm parameters' doMixedArithmetic doMixedArithmetic: sendMouseWheelEvents sendMouseWheelEvents: vmParameterAt: vmParameterAt:put: wordSize)
('miscelaneous' voidCogVMState)
!


!MethodDictionary class reorganize!
('[mkClassAndMethodCreation] instance creation')
('initialization' rehashAllInstances)
('instance creation' new newWithRoomForMoreThan:)
!

