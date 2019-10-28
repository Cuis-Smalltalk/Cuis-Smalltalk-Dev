'From Cuis 5.0 [latest update: #3931] on 26 October 2019 at 11:50:18 pm'!

!Utilities class methodsFor: 'identification' stamp: 'HAW 10/26/2019 23:43:23'!
changeStampField
	
	^' stamp: ', self changeStamp printString.! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 23:49:20'!
definitionPreamble

	^'classDefinition: ', self name printString, ' category: ', self category printString, Utilities changeStampField! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/26/2019 23:45:14'!
classRemoved: aClass fromCategory: aCategoryName

	| classDefinition |
	
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self 
				logChange: classDefinition 
				preamble: 'classRemoval: ', aClass name printString, Utilities changeStampField ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/26/2019 23:45:25'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [ self 
			logChange: aMethod sourceCode 
			preamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField ]. 
! !


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: addSelector:withMethod:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: localBindingOf: moveInstVarNamed:to:after: noteCompilationOf:meta: wantsChangeSetLogging wantsRecompilationProgressReported)
('*TDDGuruFirstBootstraping')
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

