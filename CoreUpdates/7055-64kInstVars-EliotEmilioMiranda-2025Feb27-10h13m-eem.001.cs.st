'From Cuis7.3 [latest update: #7054] on 27 February 2025 at 10:46:12 am'!

!BytecodeEncoder class methodsFor: 'behavior support' stamp: 'eem 2/27/2025 07:39'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^self subclassResponsibility! !


!EncoderForSistaV1 class methodsFor: 'behavior support' stamp: 'eem 2/27/2025 07:40'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^65536! !


!EncoderForV3PlusClosures class methodsFor: 'behavior support' stamp: 'eem 2/27/2025 07:41'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^256! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'eem 2/27/2025 10:21:25'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables.
	 This depends on the bytecode set."
	^CompiledMethod preferredBytecodeSetEncoderClass maxAccessibleInstSize! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'eem 2/27/2025 10:21:52'!
maxInstSize
	"Answer the maximum number of named instance variables
	 (as opposed to indexed variables) of the receiver.
	 The class format is
		<5 bits inst spec><16 bits inst size>"
	^16rFFFF! !


!ClassBuilder methodsFor: 'validation' stamp: 'eem 2/27/2025 10:18:23'!
checkInstSize: instSize errorMessage: errorMessage
	instSize > self class maxInstSize ifTrue:
		[self error: errorMessage.
		 ^false].
	instSize > self class maxAccessibleInstSize ifTrue:
		[Smalltalk isDevelopmentEnvironmentPresent ifTrue:
			[^self confirm: 'To access ', instSize printString, ' instance variables\requires the SistaV1 bytecode set.\See #preferredBytecodeSetEncoderClass:.' withNewLines].
		 ^false].
	^true! !


!Behavior methodsFor: 'accessing' stamp: 'eem 2/18/2020 19:37'!
instSpec
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (Context BlockClosure AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable	(DoubleWordArray et al)
		10-11	= 32-bit indexable	(WordArray et al)			(includes one odd bit, unused in 32-bit instances)
		12-15	= 16-bit indexable	(DoubleByteArray et al)		(includes two odd bits, one unused in 32-bit instances)
		16-23	= 8-bit indexable	(ByteArray et al)			(includes three odd bits, one unused in 32-bit instances)
		24-31	= compiled code	(CompiledCode et al)		(includes three odd bits, one unused in 32-bit instances)

	 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
	 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
	 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
	 has format 18 in 32 bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
	 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6).
	 Formats 24-31 are for compiled code which is a combination of pointers and bytes.  The number of pointers is
	 determined by literal count field of the method header, which is the first field of the object and must be a SmallInteger. 
	 The literal count field occupies the least significant 15 bits of the method header, allowing up to 32,767 pointer fields,
	 not including the header."
	^(format bitShift: -16) bitAnd: 16r1F! !


!ClassBuilder methodsFor: 'class format' stamp: 'eem 2/27/2025 10:23:27'!
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

!ClassBuilder methodsFor: 'validation' stamp: 'eem 2/27/2025 10:33:01'!
validateSubclassFormat: newType from: oldClass forSuper: newSuper extra: newInstSize
	"Validate the # of instVars and the format of the subclasses"
	| deltaSize errorMessage |
	oldClass ifNil: [^ true]. "No subclasses"
	"Compute the # of instvars needed for all subclasses"
	deltaSize := newInstSize.
	oldClass
		ifNotNil: [deltaSize := deltaSize - oldClass instVarNames size].
	newSuper
		ifNotNil: [deltaSize := deltaSize + newSuper instSize].
	(oldClass notNil and: [oldClass superclass notNil]) 
		ifTrue: [deltaSize := deltaSize - oldClass superclass instSize].
	errorMessage := 'more than ', self class maxInstSize printString, ' instance variables'.
	oldClass
		ifNil: [^self checkInstSize: deltaSize errorMessage: errorMessage capitalized].

	oldClass withAllSubclassesDo:
		[ :sub |
		(self
			checkInstSize: sub instSize + deltaSize
			errorMessage: sub name,' has ', errorMessage) ifFalse:
			[^false].

		"If we get this far, check whether the immediate subclasses of oldClass can keep its layout."
		(newType ~~ #normal) 
			ifTrue: [ self validateSubclass: sub canKeepLayoutFrom: oldClass forSubclassFormat: newType ]].

	^ true! !


!Behavior reorganize!
('accessing' classDepth decompilerClass format instSpec name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instSize instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: maxAccessibleInstSize maxInstSize sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteSubclassesCollection removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('creating method dictionary' methodDictionary:)
!

