'From Cuis 4.2 of 25 July 2013 [latest update: #2884] on 16 August 2016 at 9:33:18 am'!

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/16/2016 09:23:01'!
computeFormat: type instSize: newInstSize forSuper: newSuper
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	"Only for Spur!!"

	| instSize isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue:
		[newInstSize > 0 ifTrue:
			[self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	instSize := newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize]).
	instSize > 65535 ifTrue:
		[self error: 'Class has too many instance variables (', instSize printString,')'.
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
	(isPointers not and: [instSize > 0]) ifTrue:
		[self error: 'A non-pointer class cannot have named instance variables'.
		^nil].
	^self format: instSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak! !

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/16/2016 09:25:14'!
format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak

	"Only for Spur!!"

	"Compute the format for the given instance specfication.
	 Above Cog Spur the class format is
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

!ClassBuilder methodsFor: 'public' stamp: 'jmv 8/16/2016 08:47:36'!
superclass: aClass
	ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	"Pre Spur does not support Ephemerons."
	Smalltalk isSpur ifFalse: [
		self halt: 'Attempt to create a new Ephemeron Class in a PreSpur image!!'.
		^ self superclass: aClass
			subclass: t instanceVariableNames: f 
			classVariableNames: d poolDictionaries: s category: cat ].

	aClass isPointers ifFalse:
		[^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	aClass instSize + f substrings size < 2 ifTrue:
		[^self error: 'cannot make an ephemeron class with less than two named instance varaibles'].
	^self 
		name: t
		subclassOf: aClass
		type: #ephemeron
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 8/16/2016 08:44:49'!
superclass: aClass
	immediateSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a
	 new immediate class as a subclass of an existing class."

	"Pre Spur does not support creating new immediate classes."
	Smalltalk isSpur ifFalse: [
		^ self superclass: aClass
			subclass: t instanceVariableNames: f 
			classVariableNames: d poolDictionaries: s category: cat ].

	aClass instSize > 0
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with named fields'].
	aClass isVariable
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with indexed instance variables'].
	aClass isPointers
		ifFalse: [^self error: 'cannot make an immediate subclass of a class without pointer fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #immediate
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat! !


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

!Behavior methodsFor: 'private' stamp: 'jmv 8/16/2016 09:31:21'!
becomeCompactSimplyAt: index
	"Make me compact, but don't update the instances.  For importing segments."
"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct |

	Smalltalk isSpur ifTrue: [^ self halt: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self halt: 'You must not make a weak class compact'].
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name , 'is already compact'].
	(cct at: index) ifNotNil: [^ self halt: 'compact table slot already in use'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format _ format + (index bitShift: 11).
	"Caller must convert the instances"
! !

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


!ClassDescription methodsFor: 'initialization' stamp: 'jmv 7/29/2016 17:33:01'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
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

!ClassDescription methodsFor: 'initialize-release' stamp: 'eem 2/2/2015 10:42'!
updateInstances: oldInstances from: oldClass isMeta: isMeta
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver,
	 which is a newly changed class. Permute variables as necessary, and forward old instances
	 to new instances.  Answer nil to defeat old clients that expect an array of old instances.
	 The old behaviour, which necessitated a global GC, exchanged identities and answered
	 the old instances.  But no clients used the result.  This way we avoid the unnecessary GC,"
	| map variable instSize newInstances |

	oldInstances isEmpty ifTrue:
		[^nil]. "no instances to convert"
	isMeta ifTrue:
		[(oldInstances size = 1
		  and: [self soleInstance class == self
				or: [self soleInstance class == oldClass]]) ifFalse:
			[^self error: 'Metaclasses can only have one instance']].
	map := self instVarMappingFrom: oldClass.
	variable := self isVariable.
	instSize := self instSize.
	newInstances := Array new: oldInstances size.
	1 to: oldInstances size do:
		[:i|
		newInstances
			at: i
			put: (self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize map: map)].
	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsForwardIdentityTo: newInstances.
	^nil! !


!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/16/2016 09:27:20'!
computeFormat: type instSize: newInstSize forSuper: newSuper ccIndex: ccIndex
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	| instSize isVar isWords isPointers isWeak |

	"Spur uses this version"
	Smalltalk isSpur ifTrue: [
		^ self computeFormat: type instSize: newInstSize forSuper: newSuper ].

	"This for preSpur images"
	type == #compiledMethod
		ifTrue:[^CompiledMethod format].
	instSize := newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize]).
	instSize > 254 ifTrue:[
		self error: 'Class has too many instance variables (', instSize printString,')'.
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isWords := isPointers := true].
	type == #bytes ifTrue:[isVar := true. isWords := isPointers := isWeak := false].
	type == #words ifTrue:[isVar := isWords := true. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := isWords := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isWords := isPointers := true].
	(isPointers not and:[instSize > 0]) ifTrue:[
		self error:'A non-pointer class cannot have instance variables'.
		^nil].
	^(self format: instSize 
		variable: isVar 
		words: isWords 
		pointers: isPointers 
		weak: isWeak) + (ccIndex bitShift: 11)! !

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 8/16/2016 09:26:26'!
format: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak

	"Only for preSpur!!"

	"Compute the format for the given instance specfication."
	| cClass instSpec sizeHiBits fmt |
"
	NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
	For now the format word is...
		<2 bits=instSize//64><5 bits=cClass><4 bits=instSpec><6 bits=instSize\\64><1 bit=0>
	But when we revise the image format, it should become...
		<5 bits=cClass><4 bits=instSpec><8 bits=instSize><1 bit=0>
"
	sizeHiBits _ (nInstVars+1) // 64.
	cClass _ 0.  "for now"
	instSpec _ isWeak
		ifTrue:[4]
		ifFalse:[isPointers
				ifTrue: [isVar
						ifTrue: [nInstVars>0 ifTrue: [3] ifFalse: [2]]
						ifFalse: [nInstVars>0 ifTrue: [1] ifFalse: [0]]]
				ifFalse: [isWords ifTrue: [6] ifFalse: [8]]].
	fmt _ sizeHiBits.
	fmt _ (fmt bitShift: 5) + cClass.
	fmt _ (fmt bitShift: 4) + instSpec.
	fmt _ (fmt bitShift: 6) + ((nInstVars+1)\\64).  "+1 since prim size field includes header"
	fmt _ (fmt bitShift: 1). "This shift plus integer bit lets wordSize work like byteSize"
	^fmt! !

!ClassBuilder methodsFor: 'validation' stamp: 'ar 7/13/2009 21:19'!
validateSubclass: subclass canKeepLayoutFrom: oldClass forSubclassFormat: newType 
	"Returns whether the immediate subclasses of oldClass can keep its layout"
	"Note: Squeak does not appear to model classFormat relationships.. so I'm putting some logic here. bkv 4/2/2003"
	
	"Only run this test for a real subclass - otherwise this prevents changing
	a class from #subclass: to #variableSubclass: etc."
	subclass = oldClass ifTrue:[^true].

	 "isWeak implies isVariant"					
	 (oldClass isVariable and: [ subclass isWeak ])
		ifFalse: [ "In general we discourage format mis-matches"
				  (subclass typeOfClass == newType) 
				   	ifFalse: [ self error: subclass name,' cannot be recompiled'.
							  ^ false ]].
	^ true! !

!ClassBuilder methodsFor: 'private' stamp: 'gk 2/28/2005 16:35'!
reservedNames
	"Return a list of names that must not be used for variables"
	^#('self' 'super' 'thisContext' 'true' 'false' 'nil' 
		self super thisContext #true #false #nil).! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'eem 8/21/2015 11:13'!
reshapeClass: oldClass toSuper: newSuper
	"Reshape the given class to the new super class. Recompile all the methods in the newly created class. Answer the new class."
	| instVars |
	instVars := instVarMap at: oldClass name ifAbsent: [oldClass instVarNames].

	^self newSubclassOf: newSuper 
			type: oldClass typeOfClass 
			instanceVariables: instVars 
			from: oldClass! !


!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'fbs 6/5/2013 08:53'!
checkClassHierarchyConsistency
	"Check the consistency of the class hierarchy. The class hierarchy is consistent if the following
	two logical equivalences hold for classes A and B:
	- B is obsolete and 'B superclass' yields A  <-->  'A obsoleteSubclasses' contains B
	- B is not obsolete and 'B superclass' yields A  <-->  'A subclasses' contains B"
	self informUserDuring:[:bar|
		self checkClassHierarchyConsistency: bar.
	].! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'fbs 6/5/2013 08:53'!
cleanupAndCheckClassHierarchy
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary.
	Afterwards it checks whether the hierarchy is really consistent."
	self informUserDuring:[:bar|
		self cleanupAndCheckClassHierarchy: bar.
	].
! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'fbs 7/22/2013 11:43'!
cleanupClassHierarchy
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary."
	self informUserDuring:[:bar|
		self cleanupClassHierarchy: bar.
	].! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/16/2016 09:30:26'!
cleanCompactObsoleteClasses

	| cct |
	"No Compact Classes support in Spur"
	Smalltalk isSpur ifTrue: [ ^ self ].

	cct _ Smalltalk compactClassesArray.
	cct do: [ :c |
		c ifNotNil: [
			c isObsolete ifTrue: [ 
				cct at: c indexIfCompact put: nil ]]]! !

!methodRemoval: ClassBuilder #preSpurFormat:variable:words:pointers:weak:!
ClassBuilder removeSelector: #preSpurFormat:variable:words:pointers:weak:!
!methodRemoval: ClassBuilder #spurFormat:variable:words:pointers:weak:!
ClassBuilder removeSelector: #spurFormat:variable:words:pointers:weak:!

!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('method dictionary' allMethodsInCategory:)
('system-support' allUnreferencedClassVariables)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('release' obsolete)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
!

