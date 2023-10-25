'From Cuis 6.0 [latest update: #6054] on 24 October 2023 at 12:02:00 pm'!

!Metaclass methodsFor: 'accessing variables' stamp: 'jmv 10/24/2023 10:07:27'!
allClassInstVarNames
	"Similar to #allInstVarNames, but don't include class instance variables used by the system itself,
	only those defined as class side #instanceVariableNames: "

	^(superclass notNil and: [ superclass isMeta ])
		ifTrue: [superclass allClassInstVarNames , self instVarNames]
		ifFalse: [self instVarNames copy]	"Guarantee a copy is answered."! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 10/24/2023 10:06:11'!
allInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter."

	^superclass
		ifNotNil: [superclass allInstVarNames , self instVarNames]
		ifNil: [self instVarNames copy]	"Guarantee a copy is answered."! !


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing variables' allUnreferencedInstanceVariables)
('pool variables' classPool)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
!


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('tdd' testCaseClass)
('accessing variables' allClassInstVarNames allClassVarNames)
('printing' printNameOn:)
!

