'From Cuis 5.0 [latest update: #4242] on 29 June 2020 at 1:45:03 pm'!

!Behavior methodsFor: 'accessing' stamp: 'HAW 6/28/2020 10:57:39'!
theNonMetaClass

	self subclassResponsibility ! !

!Behavior methodsFor: 'printing' stamp: 'HAW 6/28/2020 12:25:41'!
printSubclassesOn: aStream level: level
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times."
	| subclassNames |
	aStream newLineTab: level.
	aStream nextPutAll: self name.
	aStream
		 space;
		 print: self instVarNames.
	self == Class ifTrue: [
		aStream
			 newLineTab: level + 1;
			 nextPutAll: '[ ... all the Metaclasses ... ]'.
		^ self ].
	subclassNames _ self subclasses asArray sort: [ :c1 :c2 |
		c1 name <= c2 name ].
	"Print subclasses in alphabetical order"
	subclassNames do: [ :subclass |
		subclass
			printSubclassesOn: aStream
			level: level + 1 ].! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 6/28/2020 12:05:30'!
subclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	^self subclasses do: aBlock! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 6/29/2020 13:06:24'!
subclassesDoGently: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."

	^self subclassesDo: aBlock! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 6/28/2020 12:20:34'!
recoverFromMDFaultWithTrace

	self subclassResponsibility ! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'HAW 6/28/2020 12:22:58'!
ultimateSourceCodeAt: selector ifAbsent: aBlock
	"Return the source code at selector, deferring to superclass if necessary"

	^ self
		sourceCodeAt: selector
		ifAbsent: [
			superclass
				ifNil: [aBlock value]
				ifNotNil: [superclass ultimateSourceCodeAt: selector ifAbsent: aBlock]]! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 6/28/2020 11:54:26'!
allBroadAccessesTo: instVarName

	| references instVarIndex definingClass |

	definingClass _ self whichClassDefinesInstanceVariable: instVarName ifNone: [ ^#() ].
	instVarIndex _ self indexOfInstanceVariable: instVarName.
	references _ SortedCollection sortBlock: [ :left :right | left stringVersion <= right stringVersion ].

	definingClass withAllSubclassesDo: [ :class | class addReferencesOf: instVarName at: instVarIndex to: references ].

	^ references ! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'HAW 6/28/2020 12:18:42'!
changeRecordsAt: selector
	"Return a list of ChangeRecords for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	"(Pen changeRecordsAt: #go:) collect: [:cRec | cRec string]"
	| aList |
	aList _ VersionsBrowser new
			scanVersionsOf: (self compiledMethodAt: selector ifAbsent: [^ nil])
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector.
	^ aList ifNotNil: [aList changeList]! !

!ClassDescription methodsFor: 'accessing instances and variables' stamp: 'HAW 6/28/2020 12:13:50'!
allUnreferencedInstanceVariables

	"Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses"

	^ self allInstVarNames reject: [ :instanceVariableName | | definingClass |
		definingClass _ self classThatDefinesInstanceVariable: instanceVariableName.
		definingClass isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]! !

!ClassDescription methodsFor: 'pool variables' stamp: 'HAW 6/28/2020 12:30:46'!
classPool

	self subclassResponsibility ! !


!Class methodsFor: 'private' stamp: 'jmv 6/29/2020 13:02:26'!
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

!Class methodsFor: 'private' stamp: 'jmv 6/29/2020 13:03:05'!
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

!Class methodsFor: 'private' stamp: 'jmv 6/29/2020 13:02:15'!
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


!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 6/29/2020 13:18:25'!
allRegularInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The
	Array ordering is the order in which the variables are stored and
	accessed by the interpreter.

	Quite like asking #allInstVarNames, but do not include Behavior state (i.e. Smalltalk internals)"

	^ (self == ProtoObject class or: [ superclass isNil ])
		ifTrue: [self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [superclass allRegularInstVarNames , self instVarNames].! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 6/28/2020 12:34:05'!
fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream.  If
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.changes file, and should only write a preamble for every method."

	| org categories |

	org _ self organization.
	categories _ org categories.

	categories ifNotNil: [ categories do: [ :cat |  | sels |
		sels _ (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
		sels do: [:sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream moveSource: moveSource toFile: fileIndex]]]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 6/28/2020 12:38:48'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver on aFileStream. If the boolean
	argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the
	source code."

	| categories |

	aFileStream nextPut: $!!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	categories := self organization categories.
	categories ifNotNil: [ categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]]! !


!Class methodsFor: 'class variables' stamp: 'HAW 6/28/2020 16:34:20'!
removeClassVarName: aString
	"Remove the class variable whose name is the argument, aString, from
	the names defined in the receiver, a class. Create an error notification if
	aString is not a class variable or if it is still being used in the code of
	the class."

	| aSymbol |
	aSymbol _ aString asSymbol.
	(classPool isNil or: [(classPool includesKey: aSymbol) not])
		ifTrue: [ ^self error: aString, ' is not a class variable'].
	self withAllSubclasses do:[:subclass |
		(Array with: subclass with: subclass class) do: [ :classOrMeta |
			(classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
				isEmpty ifFalse: [
					InMidstOfFileinNotification signal ifTrue: [
						Transcript newLine; show: self name, ' (' , aString , ' is Undeclared) '.
						^Undeclared declare: aSymbol from: classPool ].
					(self confirm: (aString,' is still used in code of class ', classOrMeta name,
						'.\Is it okay to move it to Undeclared?') withNewLines)
						ifTrue: [ ^Undeclared declare: aSymbol from: classPool ]
						ifFalse: [ ^self ]]]].
	classPool removeKey: aSymbol.
	classPool isEmpty ifTrue: [ classPool _ nil ]! !

!Class methodsFor: 'pool variables' stamp: 'HAW 6/28/2020 16:39:55'!
removeSharedPool: aDictionary
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool
	dictionaries. Create an error notification if the dictionary is not one of
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |

	(sharedPools isNil or: [(sharedPools includes: aDictionary) not ])
		ifTrue: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [:class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [sharedPools remove: aDictionary.
				sharedPools isEmpty ifTrue: [sharedPools _ nil].
				^self].

	"second get all the subclasses that reference aDictionary through me rather than a
	superclass that is one of my subclasses."

	workingSet _ self subclasses asOrderedCollection.
	satisfiedSet _ Set new.
	[workingSet isEmpty] whileFalse:
		[aSubclass _ workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse:
				[satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do:
		[:sub |
		aDictionary associationsDo:
			[:aGlobal |
			(sub whichSelectorsReferTo: aGlobal) isEmpty
				ifFalse: [^self error: aGlobal key
								, ' is still used in code of class '
								, sub name]]].
	sharedPools remove: aDictionary.
	sharedPools isEmpty ifTrue: [sharedPools _ nil]! !

!Class methodsFor: 'release' stamp: 'HAW 6/28/2020 16:32:41'!
removeFromSystem: logged
	"Forget the receiver from the Smalltalk global dictionary. Any existing
	instances will refer to an obsolete version of the receiver."

	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to unload itself"
	self unload.
	self superclass ifNotNil: [:aSuperclass |
		"If we have no superclass there's nothing to be remembered"
		aSuperclass addObsoleteSubclass: self].
	Smalltalk forgetClass: self logged: logged.
	self obsolete.! !

!Metaclass methodsFor: 'accessing instances and variables' stamp: 'HAW 6/28/2020 11:38:18'!
allClassVarNames

	"Metaclasses do not define class vars - Hernan"

	^superclass allClassVarNames! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'HAW 6/28/2020 11:34:48'!
allClassVarNames
	"Answer a Set of the names of the receiver's and the receiver's ancestor's
	class variables."

	self subclassResponsibility ! !

!methodRemoval: ClassDescription #subclassesDo: stamp: 'jmv 6/29/2020 13:07:15'!
ClassDescription removeSelector: #subclassesDo:!
!methodRemoval: ClassDescription #printSubclassesOn:level: stamp: 'jmv 6/29/2020 13:07:44'!
ClassDescription removeSelector: #printSubclassesOn:level:!
!methodRemoval: ClassDescription #ultimateSourceCodeAt:ifAbsent: stamp: 'jmv 6/29/2020 13:04:37'!
ClassDescription removeSelector: #ultimateSourceCodeAt:ifAbsent:!
!methodRemoval: Behavior #becomeUncompact stamp: 'jmv 6/29/2020 13:03:21'!
Behavior removeSelector: #becomeUncompact!
!methodRemoval: Behavior #becomeCompact stamp: 'jmv 6/29/2020 13:03:21'!
Behavior removeSelector: #becomeCompact!
!methodRemoval: Behavior #allUnreferencedInstanceVariables stamp: 'jmv 6/29/2020 13:09:27'!
Behavior removeSelector: #allUnreferencedInstanceVariables!
!methodRemoval: Behavior #becomeCompactSimplyAt: stamp: 'jmv 6/29/2020 13:03:21'!
Behavior removeSelector: #becomeCompactSimplyAt:!
!methodRemoval: Behavior #changeRecordsAt: stamp: 'jmv 6/29/2020 13:11:17'!
Behavior removeSelector: #changeRecordsAt:!
!methodRemoval: Behavior #allSharedPools stamp: 'jmv 6/29/2020 13:24:01'!
Behavior removeSelector: #allSharedPools!

!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format name parserClass sourceCodeTemplate theNonMetaClass typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary hasChangedComparedTo: instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printSubclassesOn:level: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesPreviousTo: allSuperclassesUpTo: subclasses subclassesDo: subclassesDoGently: superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesPreviousTo: withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelectorSilently:withMethod: allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: lookupSelector: methodDict methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamed: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instVarNames instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools someInstance unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('auto complete' typeName)
('methods' addMethodsTo:thatReferenceTo:special:byte: addTo:methodsThatReferenceInHierarchyTo:special:byte: closuresInfoAt: methodNodeFor: methodNodeFor:noPattern: rejectSelectorsFrom:thatReferenceTo:byte: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
!


!Class reorganize!
('accessing' name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools classPool classPool: classPoolFrom: removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' becomeCompact becomeCompactSimplyAt: becomeUncompact setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
!
