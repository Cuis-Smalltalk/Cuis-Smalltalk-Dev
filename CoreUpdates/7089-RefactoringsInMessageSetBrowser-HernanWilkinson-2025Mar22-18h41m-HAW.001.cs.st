'From Cuis7.3 [latest update: #7084] on 22 March 2025 at 9:30:55 pm'!

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:06:56'!
addAsSubclassResponsibilitySelector

	AddAsSubclassResponsibilityApplier applyFrom: self.! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:11:44'!
addParameter

	RefactoringApplier addParameterApplier applyFrom: self
	! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:16:46'!
changeKeywordOrder
	
	ChangeKeywordsSelectorOrderApplier applyFrom: self.! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:17:48'!
inlineMethod

	RefactoringApplier inlineMethodApplier applyFrom: self! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:19:52'!
moveToInstanceOrClassMethod

	MoveToInstanceOrClassMethodApplier applyFrom: self! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:20:58'!
pushDownSelectorToOneSubclass
	
	PushDownMethodToOneSubclassApplier applyFrom: self.! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:22:02'!
pushDownSelectorToSubclasses
	
	PushDownMethodToSubclassesApplier applyFrom: self.! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:23:18'!
pushUpSelector

	PushUpMethodApplier applyFrom: self.! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:24:17'!
removeParameter

	RefactoringApplier removeParameterApplier applyFrom: self
	! !

!MethodSet methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:26:13'!
renameSelector

	RefactoringApplier renameSelectorApplier applyFrom: self
	! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 3/22/2025 19:00:22'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 				false)
		#(10	'Browse Full (b)' 				$b 	window browseMethodFull 						editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 		$h 	window browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 			$O 	window openSingleMessageBrowser 				scriptIcon)
		#(40	'Browse Protocol (p)' 			$p 	window browseFullProtocol 						spreadsheetIcon)
		#(45	'Browse Package' 			nil 	window browsePackage 							packageIcon)
		#(50 	'-----')
		#(60	'File Out' 					nil 	model 	fileOutMessage 							fileOutIcon)
		#(61	'File Out All' 					nil 	model 	fileOutAllMethods 							fileOutIcon)
		#(70	'Run Test (t)' 				$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(103	'Refactorings' 				nil 	nil 		1000 										listAddIcon)
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'Refactoring' SubMenu"
		#(1010	'Rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'Change keyword Order...' 				nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'Add Parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'Remove Parameter... (S)' 				$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'Inline Method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'Move to Instance/Class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsibility' nil model 		addAsSubclassResponsibilitySelector 		goTopIcon)
		#(1100	'Push Down to Subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push Down to One Subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

		" 'more' SubMenu"
		#(2010	'Sample instance' 			nil 	window makeSampleInstance 						instanceIcon)
		#(2020	'Inspect instances' 			nil 	window inspectInstances 							inspectIcon)
		#(2030	'Inspect subinstances' 		nil 	window inspectSubInstances 						inspectIcon)
		#(2040 	'-----')
		#(2050 	'Remove from this Browser' 	nil 	model 	removeMessageFromBrowser 				listRemoveIcon)
		#(2060 	'Filter message list...' 			nil 	window filterMessageList 							findIcon)
		#(2070	'Change category...' 			nil 	model 	changeCategory 							saveAsIcon)
		#(2080	'Change sets with this Method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'Revert to previous Version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'Sort by class Hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'Sort by class Name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'Sort by Selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'Sort by Date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'Reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).
! !


!TransferMethod methodsFor: 'accessing' stamp: 'HAW 3/22/2025 21:12:26'!
selector
	
	^method selector ! !


!PushUpMethod methodsFor: 'accessing' stamp: 'HAW 3/22/2025 21:17:10'!
selector
	
	^methods anyOne selector! !


!ScopedRefactoringApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:35:49'!
applyFrom: aCodeProvider

	self subclassResponsibility! !


!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:35:08'!
applyFrom: aCodeProvider
	
	aCodeProvider selectedMessageName ifNotNil: [ :oldSelector |
		self
			createAndValueHandlingExceptionsOn: aCodeProvider
			for: oldSelector
			in: aCodeProvider selectedClassOrMetaClass ].! !


!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:30:43'!
applyFrom: aCodeProvider
	
	aCodeProvider selectedMessageName ifNotNil: [ :selectorToInline |
		self
			createAndValueHandlingExceptionsOn: aCodeProvider 
			forMethod: (MethodReference
				class: aCodeProvider selectedClassOrMetaClass 
				selector: selectorToInline )]! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:41:15'!
on: aModel forMessageSend: aMessageNodeReference

	^self new initializeOn: aModel forMessageSend: aMessageNodeReference
	! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:41:41'!
on: aModel forMethod: aMethodReference

	"To be called when calling from method selector or when inlining self method from the code editor"

	^self new initializeOn: aModel for: aMethodReference selector in: aMethodReference actualClass.! !


!TransferMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:37:51'!
applyFrom: aCodeProvider

	aCodeProvider selectedMessageName ifNotNil: [ :selectedSelector |
		(self 
			on: aCodeProvider 
			for: aCodeProvider selectedClassOrMetaClass>>selectedSelector) value ].
	! !


!AddAsSubclassResponsibilityApplier methodsFor: 'refactoring - changes' stamp: 'HAW 3/22/2025 21:19:37'!
informChangesToBrowser

! !


!PushDownMethodToSubclassesApplier methodsFor: 'as yet unclassified' stamp: 'HAW 3/22/2025 21:24:46'!
informChangesToBrowser

	browser messageListIndex: browser messageListIndex ! !


!MessageNode methodsFor: 'source ranges' stamp: 'HAW 3/22/2025 20:02:20'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges := self isCascade
		ifTrue: [ aSourceRange ] "not expanded because expansion is handled in CascadeNode"
		ifFalse: [ originalReceiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver := self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode! !


!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:07:21'!
addAsSubclassResponsibilitySelector

	AddAsSubclassResponsibilityApplier applyFrom: self.! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:15:39'!
addParameter

	RefactoringApplier addParameterApplier applyFrom: self
	! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:17:18'!
changeKeywordOrder

	ChangeKeywordsSelectorOrderApplier applyFrom: self.! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:19:04'!
inlineMethod

	RefactoringApplier inlineMethodApplier applyFrom: self! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:20:15'!
moveToInstanceOrClassMethod

	MoveToInstanceOrClassMethodApplier applyFrom: self! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:21:22'!
pushDownSelectorToOneSubclass
	
	PushDownMethodToOneSubclassApplier applyFrom: self.! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:22:43'!
pushDownSelectorToSubclasses
	
	PushDownMethodToSubclassesApplier applyFrom: self.! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:23:52'!
pushUpSelector

	PushUpMethodApplier applyFrom: self.! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:25:39'!
removeParameter

	RefactoringApplier removeParameterApplier applyFrom: self
	! !

!Browser methodsFor: 'refactorings' stamp: 'HAW 3/22/2025 20:27:03'!
renameSelector

	RefactoringApplier renameSelectorApplier applyFrom: self
	! !


!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:41:15'!
createAndValueHandlingExceptionsOn: aModel forMessageSend: aMessageNodeReference

	self createAndValueHandlingExceptions: [ self on: aModel forMessageSend: aMessageNodeReference ].
! !

!InlineMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 3/22/2025 20:41:41'!
createAndValueHandlingExceptionsOn: aModel forMethod: aMethodReference

	self createAndValueHandlingExceptions: [ self on: aModel forMethod: aMethodReference ].
! !


!TransferMethodApplier methodsFor: 'refactoring - changes' stamp: 'HAW 3/22/2025 21:11:58'!
informChangesToBrowser

	browser selector: refactoring selector refactoredTo: nil! !

!methodRemoval: PushUpMethodApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:37:51'!
PushUpMethodApplier class removeSelector: #applyFrom:!
!methodRemoval: PushUpMethodApplier class #pushUpSelector: stamp: 'HAW 3/22/2025 20:23:18'!
PushUpMethodApplier class removeSelector: #pushUpSelector:!
!methodRemoval: PushDownMethodToSubclassesApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:37:51'!
PushDownMethodToSubclassesApplier class removeSelector: #applyFrom:!
!methodRemoval: PushDownMethodToSubclassesApplier class #pushDownSelectorToSubclasses: stamp: 'HAW 3/22/2025 20:22:02'!
PushDownMethodToSubclassesApplier class removeSelector: #pushDownSelectorToSubclasses:!
!methodRemoval: PushDownMethodToOneSubclassApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:37:51'!
PushDownMethodToOneSubclassApplier class removeSelector: #applyFrom:!
!methodRemoval: PushDownMethodToOneSubclassApplier class #pushDownSelectorToOneSubclass: stamp: 'HAW 3/22/2025 20:20:58'!
PushDownMethodToOneSubclassApplier class removeSelector: #pushDownSelectorToOneSubclass:!
!methodRemoval: MoveToInstanceOrClassMethodApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:37:51'!
MoveToInstanceOrClassMethodApplier class removeSelector: #applyFrom:!
!methodRemoval: MoveToInstanceOrClassMethodApplier class #moveToInstanceOrClassMethod: stamp: 'HAW 3/22/2025 20:19:52'!
MoveToInstanceOrClassMethodApplier class removeSelector: #moveToInstanceOrClassMethod:!
!methodRemoval: AddAsSubclassResponsibilityApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:37:51'!
AddAsSubclassResponsibilityApplier class removeSelector: #applyFrom:!
!methodRemoval: AddAsSubclassResponsibilityApplier class #addAsSubclassResponsibilitySelector: stamp: 'HAW 3/22/2025 20:06:56'!
AddAsSubclassResponsibilityApplier class removeSelector: #addAsSubclassResponsibilitySelector:!
!methodRemoval: InlineMethodApplier class #iapplyFrom: stamp: 'HAW 3/22/2025 20:30:45'!
InlineMethodApplier class removeSelector: #iapplyFrom:!
!methodRemoval: InlineMethodApplier class #initializeOn:forMethod: stamp: 'HAW 3/22/2025 20:41:41'!
InlineMethodApplier class removeSelector: #initializeOn:forMethod:!
!methodRemoval: InlineMethodApplier class #initializeOn:forMessageSend: stamp: 'HAW 3/22/2025 20:41:15'!
InlineMethodApplier class removeSelector: #initializeOn:forMessageSend:!
!methodRemoval: RemoveParameterApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:35:08'!
RemoveParameterApplier class removeSelector: #applyFrom:!
!methodRemoval: RenameSelectorApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:34:31'!
RenameSelectorApplier class removeSelector: #applyFrom:!
!methodRemoval: ChangeKeywordsSelectorOrderApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:34:31'!
ChangeKeywordsSelectorOrderApplier class removeSelector: #applyFrom:!
!methodRemoval: ChangeKeywordsSelectorOrderApplier class #changeKeywordOrder: stamp: 'HAW 3/22/2025 20:16:46'!
ChangeKeywordsSelectorOrderApplier class removeSelector: #changeKeywordOrder:!
!methodRemoval: ChangeSelectorKeepingParametersApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:35:08'!
ChangeSelectorKeepingParametersApplier class removeSelector: #applyFrom:!
!methodRemoval: AddParameterApplier class #applyFrom: stamp: 'HAW 3/22/2025 20:35:08'!
AddParameterApplier class removeSelector: #applyFrom:!

!MethodSet reorganize!
('accessing' contents:notifying: contentsSelection doItReceiver labelString reformulateListNoting:)
('annotation' annotation)
('message list' addMethodReference: addMethodReference:ifIncluded: messageList messageListIndex messageListIndex: removeMessageFromBrowser removeMessageFromBrowserKeepingLabel selectedMessage selectedMessageName selector:refactoredTo: sortByClassHierarchy sortByClassName sortByDate sortBySelector sortReverse)
('message functions' deleteFromMessageList: fileOutAllMethods methodCategoryChanged reformulateList removeMessage)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedStringOrText)
('breakpoints' toggleBreakOnEntry)
('testing' isEditingMethod isEditingNone)
('private' initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('shout styling' allowsStylingFor:isTextDirty: autoHighlightAllOccurrences autoHighlightString autoHighlightString:allOccurrences: justDidAutoHighlight messageSendsRangesOf: shouldAutoHighlight)
('initialization' initialize)
('run tests' debugMethodTest runMethodTest)
('system category list' selectedSystemCategory)
('refactorings' removeMethod: addAsSubclassResponsibilitySelector addParameter changeKeywordOrder inlineMethod moveToInstanceOrClassMethod pushDownSelectorToOneSubclass pushDownSelectorToSubclasses pushUpSelector removeParameter renameSelector)
!


!TransferMethod reorganize!
('initialization' initializeFor:)
('applying - private' implementSubclassResponsibilityIn: methodCategory methodCategory: moveMethod removeMethod removeMethod: subclassResponsibilitySourceCode)
('applying' apply)
('accessing' selector)
!


!ScopedRefactoringApplier class reorganize!
('instance creation' applyFrom:)
!


!ChangeSelectorApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
('instance creation' applyFrom: createAndValueHandlingExceptionsOn:for:in: on:createAndValueHandlingExceptionsOn:in:at: on:for:in:)
!


!AddParameterApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!


!ChangeSelectorKeepingParametersApplier class reorganize!
!


!ChangeKeywordsSelectorOrderApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!


!RenameSelectorApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!


!RemoveParameterApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!


!InlineMethodApplier class reorganize!
('instance creation' applyFrom: createAndValueHandlingExceptions: createAndValueHandlingExceptionsOn:forMessageSend: createAndValueHandlingExceptionsOn:forMethod: implementorsNotFoundErrorMessage on:forMessageSend: on:forMethod:)
!


!TransferMethodApplier class reorganize!
('instance creation' applyFrom: on:for:)
!


!AddAsSubclassResponsibilityApplier reorganize!
('refactoring - creation' refactoringClass)
('refactoring - changes' informChangesToBrowser)
!


!AddAsSubclassResponsibilityApplier class reorganize!
!


!MoveToInstanceOrClassMethodApplier class reorganize!
!


!PushDownMethodToOneSubclassApplier class reorganize!
!


!PushDownMethodToSubclassesApplier class reorganize!
!


!PushUpMethodApplier class reorganize!
!

