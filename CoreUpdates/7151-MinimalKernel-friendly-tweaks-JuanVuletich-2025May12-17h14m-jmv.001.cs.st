'From Cuis7.3 [latest update: #7149] on 12 May 2025 at 5:17:59 pm'!

!ClassDescription methodsFor: '[mkClassCreation] initialization' stamp: 'jmv 5/12/2025 12:01:54'!
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


!Class methodsFor: '[mkClassCreation] initialization' stamp: 'jmv 5/12/2025 17:16:17'!
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


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 5/12/2025 12:07:26'!
with: firstElement withAll: aSequenceableCollection
	"Create a new collection containing firstElement, then all the elements from aSequenceableCollection.
	Array with: 1 withAll: (10 to: 20)
	"

	| s |
	s := aSequenceableCollection size+1.
	^ (self new: s)
		at: 1 put: firstElement;
		replaceFrom: 2 to: s with: aSequenceableCollection! !


!ClassDescription reorganize!
('[mk] accessing' instVarNames theMetaClass theNonMetaClass)
('[mk] printing' printNameOn: printOn:)
('accessing' classVersion comment comment: comment:stamp: hasComment smalltalkComment)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
('[mkClassCreation] initialization' setInstVarNames:)
!


!Class reorganize!
('[mk] accessing' name)
('[mk] accesing class hierarchy' subclassesDo:)
('[mk] printing' printNameOn:)
('[mk] class variables' classPool)
('[mk] pool variables' sharedPools)
('[mkObjectCopying] copying' copy)
('testing' hasMethods isObsolete)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classPool: classPoolFrom: classVarBindingOf: classVarNames classVarValueOf: ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool:)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('subclass creation-convenience' subclass:instanceVariableNames:classVariableNames:poolDictionaries:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutAlphabetically fileOutHierarchy fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' hasSubclasses removeSubclass: subclasses superclass:)
('private' setName: spaceUsed)
('initialization' declareClassVarString: declareClassVariables: declareSharedPoolString: declareSharedPools: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('SUnit tests' testCaseClass)
('enumerating' allSubclassesSortedDo: subclassesSortedDo:)
('[mkClassCreation] initialization' addSubclass:)
!

