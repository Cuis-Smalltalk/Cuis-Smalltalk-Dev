'From Cuis 5.0 [latest update: #3937] on 28 October 2019 at 8:56:17 am'!

!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/28/2019 08:36:56'!
markAsTest: aBoolean

	isTest := aBoolean ! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/28/2019 08:32:47'!
classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName stamp |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	stamp _ self stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: tokens last 
		meta: isMeta 
		stamp: stamp.
		
	record markAsTest: (classDefinition beginsWith: TestCase name asString).
	
	^record
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/28/2019 08:35:29'!
scanClassDefinition: tokens

	| record |
	
	record _ self classDefinitionRecordFrom: tokens.
						
	self addItem: record text: 'classDefinition: ', record changeClassName.
	! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 10/28/2019 08:41:18'!
definitionPreamble

	^'classDefinition: ', self name printString, ' category: ', self category printString, Utilities changeStampField! !


!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/28/2019 08:38:08'!
file: aFile position: aPosition type: aType

	file _ aFile.
	position _ aPosition.
	type _ aType.
	
	self markAsTest: false.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: localBindingOf: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('method dictionary' allMethodsInCategory:)
('system-support' allUnreferencedClassVariables)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('release' obsolete)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('*LiveTyping-unload-private' allRawTypesUnload: instanceVariablesRawTypesUnload: methodsAllRawTypesUnload:)
('*LiveTyping-private' allSubclassesImplementationsReturnTypesOf: createHierarchyInstanceVariableTypeInfoOf:with: createInstanceVariableRawTypesFor: createInstanceVariablesRawTypes createLocalInstanceVariableTypeInfoOf:with: createMethodReturnRawTypesOf: createMethodReturnTypeInfoOf: createMethodVariableRawTypesFor:in: createMethodVariableTypeInfoOf:in:are: createRawTypesOfSize: defaultRawTypesSize initializeEachInstanceVariableRawTypes instanceVariableRawTypesSizeFor: instanceVariablesRawTypes methodReturnRawTypesSizeOf: methodVariableRawTypesSizeFor:in: printInstanceVariablesTypesOn: storeAllInstancesCurrentTypes typedDefinition)
('*LiveTyping' asTypeFor: commonSelectorsOfInstanceVariableNamed: commonSelectorsOfInstanceVariableNamed:previousTo: commonSelectorsOfInstanceVariableNamedPreviousToObject: commonSelectorsOfMethodVariableNamed:at: commonSelectorsOfMethodVariableNamed:at:previousTo: commonSelectorsOfMethodVariableNamedPreviousToObject:at: commonSupertypeOfInstanceVariableNamed: commonSupertypeOfInstanceVariableNamed:ifNoTypes: commonSupertypeOfMethodVariableNamed:at: commonSupertypeOfMethodVariableNamed:at:ifNoTypes: initializeInstanceVariablesRawTypes initializeInstanceVariablesRawTypesWithAllInstances initializeTypeInformation initializeTypeInformationWithAllInstances instanceVariablesTypes methodVariablesTypesAt: returnCommonSelectorsAt: returnCommonSelectorsAt:previousTo: returnCommonSelectorsPreviousToObjectAt: returnCommonSupertypeAt: returnCommonSupertypeAt:ifNoTypes: returnTypeInfoAt: returnTypeInfoAt:ifAbsent: returnTypeInfoAt:ifPresent:ifAbsent: returnTypesAt: returnTypesAt:ifAbsent: typeInfoOfInstanceVariableNamed: typeInfoOfInstanceVariableNamed:ifAbsent: typeInfoOfMethodVariableNamed:at: typesOfInstanceVariableAt: typesOfInstanceVariableNamed: typesOfInstanceVariableNamed:ifAbsent: typesOfMethodVariableNamed:at:)
!


!ChangeRecord reorganize!
('access' category changeClass changeClassName changeType compilerClass dateAndTime fileName fileOutOn: isMetaClassChange methodSelector prior stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: file:position:type:class:category:meta:stamp:prior: fileIn markAsTest:)
('testing' isDoIt)
('printing' printOn:)
('*TDDGuru' isTestClassChange)
!

