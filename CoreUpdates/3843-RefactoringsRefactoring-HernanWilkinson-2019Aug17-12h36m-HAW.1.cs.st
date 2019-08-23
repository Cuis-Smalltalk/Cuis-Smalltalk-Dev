'From Cuis 5.0 [latest update: #3839] on 19 August 2019 at 6:19:21 am'!

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/17/2019 16:17:55'!
withMessageSendKeywordPositionsOf: aSelector do: aMessageSendNodeBlock ifAbsent: aBlock

	| positions sortedPositions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: aMessageSendNodeBlock.

	positions isEmpty ifTrue: [ ^aBlock value ].
	sortedPositions := positions asSortedCollection.

	^sortedPositions ! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 8/19/2019 05:37:27'!
ifChangeSelectorCanBeAppliedDo: aBlock

	^(self hasUnacceptedEdits or: [morph hasEditingConflicts ])
		ifTrue: [ self inform: 'Rename, Add Parameter and Remove Parameter can not be applied when there are unsaved changes' ]
		ifFalse: aBlock! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/19/2019 05:39:02'!
addParameter: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector

	self ifChangeSelectorCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode 
			ifTrue: [ self addParameterOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector ]
			ifFalse: [ morph flash ]]
! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/19/2019 05:43:57'!
addParameterOf: aMessageNode in: aSelectedClass at: aSelectedSelector

	RefactoringApplier addParameterApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		of: aMessageNode 
		in: aSelectedClass 
		at: aSelectedSelector! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/19/2019 05:47:11'!
addParameterTo: aSelector in: aClassToRefactor 
	
	RefactoringApplier addParameterApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		for: aSelector 
		in: aClassToRefactor ! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/18/2019 20:55:50'!
contextualAddParameter
	
	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self contextualAddParameterInMethod ]! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/18/2019 20:54:15'!
contextualAddParameter: aKeyboardEvent 
	
	self contextualAddParameter.
	^true! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/18/2019 20:56:07'!
contextualAddParameterInMethod
	
	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualAddParameterOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ]		! !

!SmalltalkEditor methodsFor: 'contextual add parameter' stamp: 'HAW 8/19/2019 05:36:00'!
contextualAddParameterOf: aMethodNode in: aSelectedClass 

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self addParameter: nodeUnderCursor in: aSelectedClass at: aMethodNode selector ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifChangeSelectorCanBeAppliedDo: [ self addParameterTo: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]
! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:50:19'!
contextualRemoveParameter
	
	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self contextualRemoveParameterInMethod ]! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:49:23'!
contextualRemoveParameter: aKeyboardEvent 
	
	self contextualRemoveParameter.
	^true! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:50:49'!
contextualRemoveParameterInMethod
	
	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualRemoveParameterOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ]! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:51:33'!
contextualRemoveParameterOf: aMethodNode in: aSelectedClass 

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self removeParameter: nodeUnderCursor in: aSelectedClass at: aMethodNode selector ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifChangeSelectorCanBeAppliedDo: [ self removeParameterTo: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]
! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:52:50'!
removeParameter: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector

	self ifChangeSelectorCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode 
			ifTrue: [ self removeParameterOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector ]
			ifFalse: [ morph flash ]]
! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:53:28'!
removeParameterOf: aMessageNode in: aSelectedClass at: aSelectedSelector

	RefactoringApplier removeParameterApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		of: aMessageNode 
		in: aSelectedClass 
		at: aSelectedSelector! !

!SmalltalkEditor methodsFor: 'contextual remove parameter' stamp: 'HAW 8/19/2019 05:55:27'!
removeParameterTo: aSelector in: aClassToRefactor 
	
	RefactoringApplier removeParameterApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		for: aSelector 
		in: aClassToRefactor ! !


!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 16:37:49'!
addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords 
	
	| originalSourceCode |
	
	originalSourceCode := aMethodNode sourceText.
	insertionPoints do: [ :aPosition | | newPosition |
		newPosition := self firstNoSeparatorIndexIn: originalSourceCode startingFrom: aPosition.
		rangesToKeywords add: ((newPosition+1) to: newPosition) -> senderTrailingString ]! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 16:38:39'!
addKeywordRangesUsing: insertionPoints to: rangesToKeywords

	insertionPoints do: [ :aPosition | 
		rangesToKeywords add: (aPosition to: aPosition-1) -> senderTrailingString ]
! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 15:57:54'!
messageSendKeywordPositionsIn: aMethodNode
	
	^aMethodNode messageSendKeywordPositionsAt: index of: oldSelector ifAbsent: [ #()].! !

!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 15:58:04'!
messageSendLastPositionIn: aMethodNode
	
	^aMethodNode messageSendLastPositionsOf: oldSelector ifAbsent: [ #() ].! !


!ChangeSelectorKeepingParameters methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 15:59:10'!
messageSendSelectorKeywordPositionsIn: aMethodNode
	
	^aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].! !


!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/18/2019 12:11:04'!
keywordAndParameterPositionsIn: aMethodNode
	
	^aMethodNode messageSendKeywordAndParameterPositionsAt: parameterIndex of: oldSelector ifAbsent: [ #() ].! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 8/18/2019 20:02:30'!
registerAddParameterApplier: anAddParameterApplierClass

	self registerApplierAt: self addParameterApplierId with: anAddParameterApplierClass ! !

!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 8/18/2019 20:03:28'!
registerRemoveParameterApplier: aRemoveParameterApplierClass

	self registerApplierAt: self removeParameterApplierId with: aRemoveParameterApplierClass ! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 8/18/2019 19:59:32'!
addParameterApplier
	
	^self applierAt: self addParameterApplierId ifAbsent: [ AddParameterApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 8/18/2019 20:16:16'!
removeParameterApplier
	
	^self applierAt: self removeParameterApplierId ifAbsent: [ RemoveParameterApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 8/18/2019 20:00:08'!
addParameterApplierId

	^#addParameterApplier! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 8/18/2019 20:00:21'!
removeParameterApplierId

	^#removeParameterApplier! !

!RefactoringApplier class methodsFor: 'appliers - id' stamp: 'HAW 8/18/2019 20:01:09'!
renameSelectorApplierId

	^#renameSelectorApplier! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 8/18/2019 20:04:34'!
resetAddParameterApplier
	
	self resetApplierAt: self addParameterApplierId! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 8/18/2019 20:16:43'!
resetRemoveParameterApplier
	
	self resetApplierAt: self removeParameterApplierId ! !

!RefactoringApplier class methodsFor: 'appliers - private' stamp: 'HAW 8/18/2019 19:55:48'!
applierAt: anApplierId ifAbsent: absentBlock

	^Appliers 
		at: anApplierId 
		ifPresent: [ :anApplierName | Smalltalk classNamed: anApplierName ]
		ifAbsent: absentBlock ! !

!RefactoringApplier class methodsFor: 'appliers - private' stamp: 'HAW 8/18/2019 20:03:03'!
registerApplierAt: anApplierId with: anApplierClass

	Appliers at: anApplierId put: anApplierClass name ! !

!RefactoringApplier class methodsFor: 'appliers - private' stamp: 'HAW 8/18/2019 20:04:48'!
resetApplierAt: anApplierId

	Appliers removeKey: anApplierId ifAbsent: []! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/17/2019 22:18:41'!
createImplementors

	^IdentitySet new.! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/17/2019 22:19:08'!
createSenders

	^IdentitySet new.
! !

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/17/2019 22:41:27'!
openChangeSelectorSendersStepWindow

	ChangeSelectorSendersStepWindow openFrom: self ! !


!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 8/17/2019 22:30:43'!
createAndValueHandlingExceptionsOn: aModel for: anOldSelector in: aClassToRefactor

	self createAndValueHandlingExceptions: [ self on: aModel for: anOldSelector in: aClassToRefactor ]! !


!AddParameterApplier class methodsFor: 'instance creation' stamp: 'HAW 8/19/2019 05:42:49'!
createAndValueHandlingExceptionsOn: aModel of: aMessageNode in: aSelectedClass at: aSelectedSelector
	
	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass 
! !


!RemoveParameterApplier class methodsFor: 'instance creation' stamp: 'HAW 8/19/2019 05:43:07'!
createAndValueHandlingExceptionsOn: aModel of: aMessageNode in: aSelectedClass at: aSelectedSelector
	
	^self createAndValueHandlingExceptionsOn: aModel for: aMessageNode selector key in: aSelectedClass 
! !


!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/17/2019 16:18:44'!
messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: absentBlock

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (aMessageSendNode keywordPositionAt: anIndex) first ]
		ifAbsent: absentBlock
	! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 8/17/2019 16:19:39'!
messageSendLastPositionsOf: aSelector ifAbsent: absentBlock

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (sourceRanges at: aMessageSendNode) last ]
		ifAbsent: absentBlock
	! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 8/19/2019 05:35:49'!
contextualRenameInClassDefinition 

	self ifChangeSelectorCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 8/19/2019 05:35:49'!
contextualRenameOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode selector ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifChangeSelectorCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]


! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 8/19/2019 05:35:49'!
rename: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor name ].

	self ifChangeSelectorCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [ ^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [ ^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | variableName |
			variableName := aNodeUnderCursor key key.
			(Smalltalk classNamed: variableName) ifNotNil: [ :classToRename | ^self renameClassOn: self codeProvider for: classToRename ].
			(Smalltalk bindingOf: variableName) ifNotNil: [ ^self renameGlobalOn: self codeProvider for: variableName ] ].

		^morph flash ]! !


!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 8/19/2019 06:04:39'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar == $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar == $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar == $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar == $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar == $N ifTrue: [^ self browseClassRefs].
	aChar == $i ifTrue: [^ self methodHierarchy].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar == $o ifTrue: [^ model fileOutMessage].
			aChar == $c ifTrue: [^ model copySelector].
			aChar == $v ifTrue: [^ self browseVersions].
			aChar == $C ifTrue: [^ model showHomeCategory].
			aChar == $O ifTrue: [^ self openSingleMessageBrowser].
			aChar == $x ifTrue: [^ model removeMessage].
			aChar == $t ifTrue: [^ model runMethodTest].
			aChar == $r ifTrue: [^ model debugMethodTest].
			aChar == $R ifTrue: [^ self renameSelector].
			aChar == $U ifTrue: [^ self addParameter ].
			aChar == $I ifTrue: [^ self removeParameter ]]
		ifNil: [
			aChar == $R ifTrue: [^ model renameClass]]! !


!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 8/18/2019 20:37:30'!
addParameter

	model selectedMessageName ifNotNil: [ :oldSelector | 
		RefactoringApplier addParameterApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ]! !

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 8/18/2019 20:29:17'!
removeParameter

	model selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier removeParameterApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ].
		
		! !


!ChangeSelectorSendersStepWindow methodsFor: 'actions' stamp: 'HAW 8/18/2019 11:46:24'!
seeImplementors
	
	self delete.
	
	ChangeSelectorImplementorsStepWindow openFrom: applier! !


!AddParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 16:38:25'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| insertionPoints |
	
	isAddingLast 
		ifTrue: [ 
			insertionPoints := self messageSendLastPositionIn: aMethodNode. 
			self addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords ]
		ifFalse: [ 
			insertionPoints := self messageSendKeywordPositionsIn: aMethodNode.
			self addKeywordRangesUsing: insertionPoints to: rangesToKeywords ]! !


!ChangeSelectorKeepingParameters methodsFor: 'rename senders - private' stamp: 'HAW 8/17/2019 15:58:57'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	| oldSelectorKeywordsRanges |

	oldSelectorKeywordsRanges := self messageSendSelectorKeywordPositionsIn: aMethodNode.
	self addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords.
	
	! !


!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 8/18/2019 12:10:48'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| keywordAndParameterPositions senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := self keywordAndParameterPositionsIn: aMethodNode.
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition | | lastPosition |
		lastPosition := self lastSeparatorIndexIn: senderSourceCode startingFrom: aKeywordAndParameterPosition last.
		rangesToKeywords add: (aKeywordAndParameterPosition first to: lastPosition) -> senderReplacementString ]
	! !


!RefactoringApplier class methodsFor: 'appliers - registering' stamp: 'HAW 8/18/2019 20:03:49'!
registerRenameSelectorApplier: aRenameSelectorApplierClass

	self registerApplierAt: self renameSelectorApplierId  with: aRenameSelectorApplierClass ! !

!RefactoringApplier class methodsFor: 'appliers - getting' stamp: 'HAW 8/18/2019 20:01:43'!
renameSelectorApplier
	
	^self applierAt: self renameSelectorApplierId ifAbsent: [ RenameSelectorApplier ]! !

!RefactoringApplier class methodsFor: 'appliers - resetting' stamp: 'HAW 8/18/2019 20:15:33'!
resetRenameSelectorApplier
	
	self resetApplierAt: self renameSelectorApplierId ! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/17/2019 22:19:01'!
initializeImplementorsAndSenders

	implementors := self createImplementors.
	senders := self createSenders! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'HAW 8/18/2019 20:51:47'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Add Parameter... (U)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Remove Parameter... (I)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'HAW 8/19/2019 06:12:02'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor')
		#(		$U	#contextualAddParameter:		'Adds parameter to message that is under cursor')
		#(		$I	#contextualRemoveParameter:		'Removes parameter to message that is under cursor')
		#(		$O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable'))
! !

!methodRemoval: RenameSelectorApplier class #createAndValueHandlingExceptionsOn:for:in:!
RenameSelectorApplier class removeSelector: #createAndValueHandlingExceptionsOn:for:in:!
!methodRemoval: RenameSelectorApplier #openChangeSelectorSendersStepWindow!
RenameSelectorApplier removeSelector: #openChangeSelectorSendersStepWindow!
!methodMoveToSomePackage: ChangeSelectorApplier #sendersFrom:!
ChangeSelectorApplier removeSelectorIfInBaseSystem: #sendersFrom:!
!methodRemoval: AddParameter #messageSendKeywordPositionsOf:!
AddParameter removeSelector: #messageSendKeywordPositionsOf:!
!methodRemoval: AddParameter #messageSendLastPositionOf:!
AddParameter removeSelector: #messageSendLastPositionOf:!
!methodRemoval: ChangeSelectorSendersStepWindow #changeRequestSenders!
ChangeSelectorSendersStepWindow removeSelector: #changeRequestSenders!
!methodRemoval: ChangeSelectorSendersStepWindow #refactor!
ChangeSelectorSendersStepWindow removeSelector: #refactor!
!methodRemoval: SmalltalkEditor #ifRenameCanBeAppliedDo:!
SmalltalkEditor removeSelector: #ifRenameCanBeAppliedDo:!

!Encoder reorganize!
('visiting' accept:)
('results' allLiterals associationForClass literals tempNames tempNodes tempsAndBlockArgs undeclaredTemps unusedTempNames)
('temps' autoBind: bindAndJuggle: bindArg: bindArg:range: bindBlockArg:within: bindBlockArg:within:range: bindBlockTemp: bindBlockTemp:within: bindBlockTemp:within:range: bindTemp: bindTemp:range: fixTemp: floatTemp: maxTemp newTemp: newUndeclaredTemp:)
('encoding' cantStoreInto: doItInContextName encodeLiteral: encodeLiteral:range: encodeSelector: encodeVariable: encodeVariable:ifUnknown: encodeVariable:sourceRange:ifUnknown: litIndex: lookupVariable:ifAbsent: noteOptimizedSelector: sharableLitIndex: undeclared:)
('private' bindUndeclaredTemp: classEncoding global:name: interactive lookupInPools:ifFound: name:key:class:type:set: possibleNamesFor: possibleVariablesFor: reallyBind: warnAboutShadowed:)
('source mapping' addMultiRange:for: globalSourceRanges messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: messageSendSelectorKeywordRangesOf:ifAbsent: noteSourceRange:forNode: parameterDefinitionPositionFor: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsOfLiteralArraysContaining: rangeForNode:ifAbsent: rangesForLiteralNode:ifAbsent: rangesForLiteralVariableNode:ifAbsent: rangesForVariable:checkingType:ifAbsent: rawSourceRanges sourceMap withMessageSendKeywordPositionsOf:do:ifAbsent: withRangesOf:including:do:)
('accessing' maxIndexableLiterals maxNumLiterals methodNodeClass selector selector:)
('error handling' notify: notify:at: requestor:)
('initialization' fillDict:with:mapping:to: init:context:notifying: initScopeAndLiteralTables noteSuper temps:literals:class:)
('release' release)
('*LiveTyping' actualMessageSendKeywordAndParameterPositionsOfAll:at:of:ifAbsent:withPossibleMessageSends:using: actualMessageSendKeywordPositionsOfAll:at:ifAbsent:withPossibleMessageSends:using: actualMessageSendLastPositionsOfAll:ifAbsent:withPossibleMessageSends:using: actualMessageSendsSelectorPositionsOfAll:ifAbsent:withPossibleMessageSends:using: topParseNodeIncluding:ifAbsent: withActualMessageSendsOfAll:do:ifAbsent:withPossibleMessageSends:sortingWith:using:)
!


!MethodNode reorganize!
('visiting' accept:)
('accessing' argumentNames arguments arguments: body primitiveErrorVariableName removeProperty: tempNodes temporaries temporaries:)
('converting' decompileString preen preenLocalIfNotNilArg)
('code generation' encoder generate generate: generate:ifQuick: generate:using: generate:using:ifQuick: generatePreSpur:using: generateSpur:using: parserClass properties selector selectorNode)
('printing' classAndSelector methodClass printOn: printPragmasOn: printPrimitiveOn: printPropertiesOn: printWithClosureAnalysisOn: sourceText tempNames)
('source mapping' addPositionTo:of:inside: messageSendKeywordAndParameterPositionsAt:of:ifAbsent: messageSendKeywordPositionsAt:of:ifAbsent: messageSendLastPositionsOf:ifAbsent: messageSendSelectorKeywordPositionsOf:ifAbsent: nextPositionAfterAddPositionTo:of:startingAt: parameterDefinitionPositionAt: parseNodeIncluding:ifAbsent: positionsForInstanceVariable:ifAbsent: positionsForLiteralNode:ifAbsent: positionsForLiteralVariableNode:ifAbsent: positionsForTemporaryVariable:ifAbsent: positionsInLiteralArrayOf: positionsOf:containedIn: rangeForNode:ifAbsent: rawSourceRanges rawSourceRangesAndMethodDo: selectorKeywordPositionAt: selectorKeywordsPositions selectorLastPosition withParseNodeIncluding:do:ifAbsent:)
('primitive error codes' removeAndRenameLastTempIfErrorCode)
('debugger support' blockExtentsToTempsMap hasGeneratedMethod)
('code generation (closures)' addLocalsToPool: ensureClosureAnalysisDone locationCounter noteBlockEntry: noteBlockExit: referencedValuesWithinBlockExtent:)
('initialization' block selector: selector:arguments:precedence:temporaries:block:encoder:primitive: selector:arguments:precedence:temporaries:block:encoder:primitive:properties: selector:arguments:precedence:temporaries:block:encoder:primitive:properties:selectorKeywordsRanges: sourceText:)
('testing' hasArgumentOrTemporary: hasLocalNamed: isMultipleRanges:)
('*LiveTyping' actualMessageSendKeywordAndParameterPositionsOfAll:at:of:ifAbsent:withPossibleMessageSends: actualMessageSendKeywordPositionsOfAll:at:ifAbsent:withPossibleMessageSends: actualMessageSendLastPositionsOfAll:ifAbsent:withPossibleMessageSends: actualMessageSendsSelectorPositionsOfAll:ifAbsent:withPossibleMessageSends: identifierPrintString nodeTypePrintString topBlockTempsMap topParseNodeIncluding:ifAbsent: typesIn:addingIncompleteTypeInfoTo: withTopParseNodeIncluding:do:ifAbsent:)
('*Cuis-University-Model' sendsRecursivelyToSelf)
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
('*LiveTyping' actualImplementorsOfIt actualImplementorsOfIt: actualLocalImplementorsOfIt actualLocalImplementorsOfIt: actualSendersOfIt actualSendersOfIt: balloonTypeInfoAt: balloonTypeInfoInMethodAt: balloonTypeInfoOf:in:definedAt: balloonTypeInfoOfInstanceVariableAt: showSelectionTypeInfo showSelectionTypeInfo: showSelectionTypeInfoInMethod showTypeInfoOf:in:definedAt: showTypeInfoOfInstanceVariable typeCheckMethod typeCheckMethod:)
('contextual add parameter' addParameter:in:at: addParameterOf:in:at: addParameterTo:in: contextualAddParameter contextualAddParameter: contextualAddParameterInMethod contextualAddParameterOf:in:)
('contextual remove parameter' contextualRemoveParameter contextualRemoveParameter: contextualRemoveParameterInMethod contextualRemoveParameterOf:in: removeParameter:in:at: removeParameterOf:in:at: removeParameterTo:in:)
!


!ChangeSelector class reorganize!
('pre-conditions' assertAllImplementors:haveSame: assertAllSenders:send:)
('*LiveTyping-exceptions' errorDescriptionForReceiverTypesNotIncludedInImplementorTypes: signalReceiverTypesNotIncludedInImplementorTypes:)
('exceptions' errorMessageForInvalidImplementors: errorMessageForInvalidSenders:of: signalInvalidImplementors: signalInvalidSenders:of:)
('*LiveTyping-pre-conditions' assertReceiverTypesOfAll:areIncludedInAllTypesOf:)
('*LiveTyping' addActualImplementorsOf:forTypes:to:andActualSendersTo:inSystem: addActualImplementorsOf:in:to:andActualSendersTo:inSystem:)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of:)
!


!AddParameter reorganize!
('initialization' initializedNamed:at:addingLast:initializedWith:to:implementing:addingToImplementors:addingToSenders:implementors:senders:)
('rename senders - private' addKeywordRangesForLastPositionOf:using:to: addKeywordRangesUsing:to: addMessageSendSelectorKeywordRangesOf:to: firstNoLineSeparatorIndexIn:startingFrom: firstNoSeparatorIndexIn:startingFrom: firstNot:indexIn:startingFrom: messageSendKeywordPositionsIn: messageSendLastPositionIn:)
('create new implementors - private' implementorNewSourceCodeOf:)
!


!RefactoringApplier class reorganize!
('value handling exceptions' createAndValueHandlingExceptions:)
('initialization' initialize)
('appliers - registering' registerAddParameterApplier: registerRemoveParameterApplier: registerRenameSelectorApplier:)
('appliers - getting' addParameterApplier removeParameterApplier renameSelectorApplier)
('appliers - id' addParameterApplierId removeParameterApplierId renameSelectorApplierId)
('appliers - resetting' resetAddParameterApplier resetRemoveParameterApplier resetRenameSelectorApplier)
('appliers - private' applierAt:ifAbsent: registerApplierAt:with: resetApplierAt:)
!


!ChangeSelectorApplier reorganize!
('refactoring - parameters request' askForImplementosAndSenders askScope calculateImplementorsAndSenders createImplementors createSenders implementorsAndSendersForClass implementorsAndSendersForHierarchy implementorsAndSendersInCategory implementorsAndSendersInCategoryAndHierarchy implementorsAndSendersInSystem initializeImplementorsAndSenders openChangeSelectorSendersStepWindow scopeOptionLabels startWizard)
('accessing' implementors implementors: oldSelector senders senders: wizardStepWindow:)
('refactoring - changes' closeBrowser doNotShowChanges informChangesToBrowser messageSetWindowClass showChanges showChangesInMessageSetWindow)
('*LiveTyping' actualImplementorsAndSenders actualImplementorsAndSendersWhenNotOnMessageSend actualImplementorsAndSendersWhenOnMessageSend actualScopeOptionLabels canNotRefactorWithIncompleteTypeInfo canNotRefactorWithoutTypeInfo filterSenders handleCanNotChangeSelectorWithNotIncludedTypes: isActualScopeChoice isOnMessageSend isWithPossibleMessageSends messageNode receiverTypes selectedSelector)
('refactoring - creation' createAndApplyRefactoring createAndApplyRefactoringWhenNoSendersAndOneImplementor: refactoringClass)
('initialization' initializeOn:for:in:)
('evaluation' ifHasNoSendersAndOneImplementor:ifNot: value wizardEnded)
!


!ChangeSelectorApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
('instance creation' createAndValueHandlingExceptionsOn:for:in: on:for:in:)
!


!AddParameterApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
('instance creation' createAndValueHandlingExceptionsOn:of:in:at:)
!


!RenameSelectorApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
!


!RemoveParameterApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
('instance creation' createAndValueHandlingExceptionsOn:of:in:at:)
!

"Postscript:
Initializes editor shortcuts"
Editor initialize.
!

