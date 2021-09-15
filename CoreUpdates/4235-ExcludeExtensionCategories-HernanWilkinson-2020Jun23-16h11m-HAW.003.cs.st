'From Cuis 5.0 [latest update: #4233] on 23 June 2020 at 6:59:08 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 6/23/2020 18:58:46'!
fileOutOrganizationOn: aFileStream excludingExtensions: shouldExcludeExtensions
	"File a description of the receiver's organization on aFileStream.
	Excludes extentions categories if shouldExcludeExtensions"

	| organizationString |
	
	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: self name, ' reorganize'; newLine.
	organizationString := String streamContents: [ :aStream |
		self organization printOn: aStream excludingExtensions: shouldExcludeExtensions ].
	aFileStream nextChunkPut: organizationString; newLine! !


!Categorizer methodsFor: 'printing' stamp: 'HAW 6/23/2020 18:35:31'!
printCategory: aCategory at: aCategoryIndex with: aStartElementIndex on: aStream

	| elementIndex |
	
	elementIndex := aStartElementIndex.
	aStream nextPut: $(.
	aCategory printOn: aStream.
	
	[elementIndex <= (categoryStops at: aCategoryIndex)] whileTrue: [
		aStream space; nextPutAll: (elementArray at: elementIndex).
		elementIndex _ elementIndex + 1].
	
	aStream nextPut: $); newLine.
	
	^elementIndex ! !

!Categorizer methodsFor: 'printing' stamp: 'HAW 6/23/2020 18:57:52'!
printOn: aStream excludingExtensions: shouldExcludeExtensions
	"Refer to the comment in Object|printOn:."

	| elementIndex |
	
	elementIndex _ 1.
	1 to: categoryArray size do: [ :categoryIndex | | category |
		category _ (categoryArray at: categoryIndex) asString.
		(shouldExcludeExtensions and: [ self isPackageCategoryExtension: category ]) ifFalse: [
			elementIndex _ self printCategory: category at: categoryIndex with: elementIndex on: aStream.]]! !

!Categorizer methodsFor: 'testing' stamp: 'HAW 6/23/2020 18:55:22'!
isPackageCategoryExtension: aCategory

	^aCategory beginsWith: '*'! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 6/23/2020 18:58:23'!
fileOutOrganizationOn: aFileStream
	"File a description of the receiver's organization on aFileStream."

	self fileOutOrganizationOn: aFileStream excludingExtensions: false! !


!ClassDescription class methodsFor: 'utilities' stamp: 'HAW 6/23/2020 18:55:22'!
printPackageExtensionCategories
	"In a bare image, without any packages, should print nothing
	ClassDescription printPackageExtensionCategories
	ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories].
	"
	ClassDescription allSubInstances do: [ :class | | classOrganization |
		classOrganization _ class organization.
		classOrganization categories do: [ :category |
				(classOrganization isPackageCategoryExtension: category) ifTrue: [
					{class. category} print ]]].! !


!Categorizer methodsFor: 'printing' stamp: 'HAW 6/23/2020 18:57:52'!
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	self printOn: aStream excludingExtensions: false! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 6/23/2020 18:58:23'!
fileOutPSFor: class on: stream 
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	dict _ classRecord methodChangeTypes.
	((dict includesKey:  #initialize) and: [ class isMeta ]) ifTrue: [
		stream nextChunkPut: class soleInstance name, ' initialize'; newLine].
	((classRecord includesChangeType: #change)
			and: [(currentDef _ class definition) ~= (self fatDefForClass: class)]) ifTrue: [
		stream
			nextPut: $!!;
			nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: currentDef; newLine].
	(classRecord includesChangeType: #reorganize) ifTrue: [
		class fileOutOrganizationOn: stream excludingExtensions: true.
		stream newLine]! !

!methodRemoval: Categorizer #printOn:excludingExtentions: stamp: 'HAW 6/23/2020 18:57:52'!
Categorizer removeSelector: #printOn:excludingExtentions:!
!methodRemoval: Categorizer #isPackageCategoryExtention: stamp: 'HAW 6/23/2020 18:55:22'!
Categorizer removeSelector: #isPackageCategoryExtention:!
!methodRemoval: ClassDescription #fileOutOrganizationOn:excludingExtentions: stamp: 'HAW 6/23/2020 18:58:23'!
ClassDescription removeSelector: #fileOutOrganizationOn:excludingExtentions:!
!methodRemoval: ClassDescription #organizationExcludingExtentions: stamp: 'HAW 6/23/2020 18:37:44'!
ClassDescription removeSelector: #organizationExcludingExtentions:!

!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: printSubclassesOn:level: removeUninstantiatedSubclassesSilently subclasses subclassesDo:)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: induceMDFault methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: ultimateSourceCodeAt:ifAbsent:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: moveInstVarNamed:to:after: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: linesOfCode logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
!


!ClassDescription class reorganize!
('utilities' printPackageExtensionCategories)
!


!Categorizer reorganize!
('accessing' addCategory: addCategory:before: allMethodSelectors categories categories: categoryOfElement: changeFromCategorySpecs: changeFromString: classify:under: classify:under:suppressIfDefault: classifyAll:under: elementCategoryDict isEmptyCategoryNumber: listAtCategoryNamed: listAtCategoryNumber: moveCategoryBottom: moveCategoryDown: moveCategoryTop: moveCategoryUp: numberOfCategoryOfElement: removeCategory: removeElement: removeEmptyCategories renameCategory:toBe: sortCategories)
('printing' printCategory:at:with:on: printOn: printOn:excludingExtensions:)
('fileIn/Out' scanFrom:)
('private' elementArray firstIndexOfCategoryNumber: lastIndexOfCategoryNumber: setDefaultList:)
('copying' postCopy)
('testing' hasAnyCategoriesSuchThat: isPackageCategoryExtension:)
('tdd' classesAt: testCaseClassesAt:)
!


!ChangeSet reorganize!
('change logging' noteAddClass: noteChangeClass:from: noteClassMoveToOtherPackage: noteMethodMoveToOtherPackage:forClass: noteNewMethod:forClass:selector:priorMethod: noteRecategorizationOfClass: noteRemoveSelector:class:priorMethod:lastMethodInfo: noteRenameClass:as:)
('accessing' author codePackage codePackage: editPostscript hasPostscript hasUnsavedChanges hasUnsavedChanges: isForBaseSystem isForBaseSystem: methodChanges methodInfoFromRemoval: movedClassRecords name name: printOn: removePostscript removedClassRecords sortedClassRecords: sortedMovedClassesToOtherPackage sortedRemovedClassRecords)
('testing' canHavePreambleAndPostscript isEmpty isMoribund isWithClass: methodsWithoutClassifications okayToRemove)
('method changes' changedMessageList changedMessageListAugmented messageListForChangesWhich:ifNone: methodChangesAtClass: removeSelectorChanges:class: selectorsInClass:)
('class changes' changedClassCategories changedClassNames changedClasses classChangeAt: fatDefForClass: noteClassForgotten: noteClassStructure: noteCommentClass: noteRemovalOf:fromCategory: noteReorganizeClass: trimHistory)
('moving changes' hasPreamble methodsWithAnyInitialsOtherThan: methodsWithInitialsOtherThan: methodsWithoutComments removeClassChanges: removePreamble)
('fileIn/Out' askAddedInstVars: askRemovedInstVars: askRenames:addTo:using: assurePostscriptExists assurePreambleExists checkForSlips fileOut fileOutClassDefinitionsOf:on: fileOutClassDeletionFrom:doItOnlyIfInBaseSystem:on: fileOutClassInitializationsOf:on: fileOutMethodAdditionsFor:on: fileOutMethodChangesFor:on: fileOutMethodMovedToOtherPackagesFor:on: fileOutMethodRemovalOf:for:movedToOtherPackage:on:stamp: fileOutMethodRemovalsFor:on: fileOutMethodRemovalsOf:movedToOtherPackage:for:on: fileOutMethodsAdditionsOf:on: fileOutMethodsChangesOf:on: fileOutMovedClassRecord:on: fileOutMovedClassesOn: fileOutOn: fileOutPSFor:on: fileOutPostscriptOn: fileOutPreambleOn: fileOutRemovedAndMovedMethodsOf:on: fileOutRemovedClassRecord:on: fileOutRemovedClassesOn: objectForDataStream: postscriptString postscriptString: preambleString preambleString: preambleTemplate)
('private' addCoherency: atClass:add: atClass:includes: atSelector:class: changeRecorderFor: fileOutClassDefinition:on: oldNameFor:)
('system change notifications' aboutToRenameClass:from:to:inCategory: classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classReorganized: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('initialization' clear initialize wither)
!

