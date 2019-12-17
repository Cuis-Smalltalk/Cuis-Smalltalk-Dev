'From Cuis 5.0 [latest update: #3959] on 14 December 2019 at 12:33:24 pm'!

!Browser methodsFor: 'testing' stamp: 'HAW 12/14/2019 12:01:16'!
is: aSymbol

	^ aSymbol == #Browser or: [ super is: aSymbol ]! !

!Browser methodsFor: 'testing' stamp: 'HAW 12/14/2019 12:30:56'!
isEditingClass

	^self isEditingExistingClass or: [ self isEditingNewClass ]! !

!Browser methodsFor: 'testing' stamp: 'HAW 12/14/2019 12:29:57'!
isEditingExistingClass

	^editSelection == #editClass! !

!Browser methodsFor: 'testing' stamp: 'HAW 12/14/2019 12:14:04'!
isEditingNewClass

	^editSelection == #newClass ! !


!Browser methodsFor: 'accessing' stamp: 'HAW 12/14/2019 12:29:57'!
acceptedStringOrText
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."

	| comment theClass latestCompiledMethod |
	latestCompiledMethod _ currentCompiledMethod.
	currentCompiledMethod _ nil.

	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	self isEditingNewClass 
		ifTrue: [^ (theClass _ self selectedClass)
			ifNil: [
				Class template: selectedSystemCategory]
			ifNotNil: [
				Class templateForSubclassOf: theClass category: selectedSystemCategory]].
	self isEditingExistingClass 
		ifTrue: [^ self classDefinitionText ].
	editSelection == #editComment 
		ifTrue: [
			(theClass _ self selectedClass) ifNil: [^ ''].
			comment _ theClass comment.
			currentCompiledMethod _ theClass organization commentRemoteStr.
			^ comment size = 0
				ifTrue: ['This class has not yet been commented.']
				ifFalse: [comment]].
	editSelection == #hierarchy 
		ifTrue: [^ self selectedClassOrMetaClass printHierarchy].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [
			^ (theClass _ self selectedClassOrMetaClass) 
				ifNil: ['']
				ifNotNil: [theClass sourceCodeTemplate]].
	editSelection == #editMessage
		ifTrue: [
			self showingByteCodes ifTrue: [^ self selectedBytecodes].
			currentCompiledMethod _ latestCompiledMethod.
			^ self selectedMessage].

	self error: 'Browser internal error: unknown edit selection.'! !

!Browser methodsFor: 'accessing' stamp: 'HAW 12/14/2019 12:30:40'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment,
	 hierarchy).  Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.
	editSelection == #editSystemCategories ifTrue: [ ^ self changeSystemCategories: aString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: aString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: aText stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #hierarchy ifTrue: [ ^ true ].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: aString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: aText notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !

!Browser methodsFor: 'annotation' stamp: 'HAW 12/14/2019 12:29:57'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !

!Browser methodsFor: 'class functions' stamp: 'HAW 12/14/2019 12:30:40'!
explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits _ Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole _ lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply _ '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes _ Smalltalk allClassesImplementing: whole.
			classes _ 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #hierarchy
		ifTrue: 
			["Instance variables in subclasses"
			classes _ self selectedClassOrMetaClass allSubclasses.
			classes _ classes detect: [:each | (each instVarNames
						detect: [:name | name = string] ifNone: nil) notNil]
					ifNone: [^nil].
			classes _ classes printString.
			^'"is an instance variable in class ' , classes , '."
' , classes , ' browseAllAccessesTo: ''' , string , '''.'].
	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'HAW 12/14/2019 12:29:57'!
acceptedStringOrText
	self updateInfoView.
	(self isEditingNewClass and: [ codeFile notNil ])
		ifTrue: [ ^codeFile description ].
	self isEditingExistingClass 
		ifTrue:[ ^self modifiedClassDefinition ].
	^super acceptedStringOrText! !

!CodeFileBrowser methodsFor: 'infoView' stamp: 'HAW 12/14/2019 12:29:57'!
infoViewContents
	| theClass |
	self isEditingNewClass ifTrue: [
		^codeFile
			ifNil: [ 'No file selected' ]
			ifNotNil: [ codeFile summary ]].
	self selectedClass ifNil: [^ ''].
	theClass _ Smalltalk at: self selectedClass name asSymbol ifAbsent: nil.
	self isEditingExistingClass ifTrue: [
		^ theClass
			ifNotNil: ['Class exists already in the system']
			ifNil: ['Class not in the system']].
	editSelection == #editMessage ifFalse: [^ ''].
	(theClass notNil and: [self metaClassIndicated])
		ifTrue: [theClass _ theClass class].
	^ (theClass notNil and: [theClass includesSelector: self selectedMessageName])
		ifTrue: ['Method already exists' , self extraInfo]
		ifFalse: ['**NEW** Method not in the system']! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 12/14/2019 12:29:57'!
isEditingClassDefinition

	^(self codeProvider is: #Browser) and: [ self codeProvider isEditingExistingClass ]! !


!BrowserCommentTextMorph methodsFor: 'updating' stamp: 'HAW 12/14/2019 12:29:57'!
update: anAspect
	super update: anAspect.
	anAspect == #editSelection ifFalse: [ ^self ].
	model textProvider isEditingExistingClass
		ifTrue: [ self showPane ]
		ifFalse: [ self hidePane ]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/14/2019 12:30:40'!
parse: allSource in: contextClass and: specificModel
	
	| isMethod |
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position);
		allSource: allSource.
		
	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingClass not ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.
	
	^ parser last3Ranges.
! !

!methodRemoval: Browser #isEditingClassOrNewClass stamp: 'HAW 12/14/2019 12:30:40'!
Browser removeSelector: #isEditingClassOrNewClass!

!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' insertSuperclass removeClass renameClass renameGlobal safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingMethod isEditingNewClass)
!


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText codeFile: contents:notifying: contentsSymbol selectedCodeFile)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('class list' classList renameClass selectedClass)
('edit pane' selectedBytecodes selectedMessage)
('diffs' methodDiffFor:class:selector:meta: modifiedClassDefinition)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('menus' contentsSymbolQuints)
('shout styling' shouldStyle:with:)
!


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifChangeSelectorCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
!


!BrowserCommentTextMorph reorganize!
('displaying' hidePane showPane)
('updating' update:)
('accessing' separator:)
!


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at:with: computeMessageEntriesFor:at:in:and:with: computeMessageEntriesForClass: computeMessageEntriesForClass:with: computeMessageEntriesForClass:withPreviousBinaryMessageSend: computeMessageEntriesForClassOrNil:with: computeMessageEntriesOfCascadeReceiverAt:with: computeMessageEntriesOfEnclosedExpressionReturnAt:with: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:using:at:in:and: ifEmptyEntriesShowAllPrefixedSelectors isBinaryMessageSend:)
('identifier entries - private' computeIdentifierEntries)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
!

