'From Cuis 5.0 of 7 November 2016 [latest update: #3311] on 5 May 2018 at 8:58:04 pm'!

!CodeWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:54:23'!
offerClassListMenu
	"Offer the shifted class-list menu."

	^ self classListMenu popUpInWorld! !

!CodeWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:53:04'!
offerClassListMenu2
	"Offer the shifted class-list menu."

	^ self classListMenu2 popUpInWorld! !

!CodeWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:50:27'!
openMessageListMenu2
	"Offer the additional selector-list menu"

	^ self messageListMenu2 popUpInWorld! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:56:29'!
addExtraMenu2ItemsTo: aMenu
	"The shifted selector-list menu is being built; some menu items are appropriate only for certain kinds of browsers, and this gives a hook for them to be added as approrpiate.  If any is added here, a line should be added first -- browse reimplementors of this message for examples."! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:55:04'!
classListMenu2
	"Set up the menu to apply to the receiver's class list when the shift key is down"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addList: #(
			-
			('unsent methods'					browseUnusedMethods
				'browse all methods defined by this class that have no senders')
			('unreferenced inst vars'			showUnreferencedInstVars
				'show a list of all instance variables that are not referenced in methods')
			('unreferenced class vars'			showUnreferencedClassVars
				'show a list of all class variables that are not referenced in methods')
			('subclass template'				makeNewSubclass
				'put a template into the code pane for defining of a subclass of this class'								model)
			-
			('sample instance'					makeSampleInstance
				'give me a sample instance of this class, if possible')
			('inspect instances'					inspectInstances
				'open an inspector on all the extant instances of this class')
			('inspect subinstances'				inspectSubInstances
				'open an inspector on all the extant instances of this class and of all of its subclasses')
			-
			('create inst var accessors'			createInstVarAccessors
				'compile instance-variable access methods for any instance variables that do not yet have them'		model)
			-
			('more...'							offerClassListMenu
				'return to the standard class-list menu')).
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:56:50'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	aMenu addList: #(
		('toggle diffing (D)'						toggleDiffing							''		model)
		('implementors of sent messages'		browseAllMessages)
		-
		('local senders of...'						browseLocalSendersOfMessages)
		('local implementors of...'				browseLocalImplementors)
		-
		('spawn sub-protocol'					browseProtocol)
		('spawn full protocol'					browseFullProtocol)
		-
		('sample instance'						makeSampleInstance)
		('inspect instances'						inspectInstances)
		('inspect subinstances'					inspectSubInstances)).

	self addExtraMenu2ItemsTo: aMenu.
	aMenu addList: #(
		-
		('change category...'					changeCategory							''		model)).

	model canShowMultipleMessageCategories ifTrue: [ aMenu addList: #(
		('show category (C)'					showHomeCategory						''		model))].
	aMenu addList: #(
		-
		('change sets with this method'			findMethodInChangeSets)
		('revert to previous version'			revertToPreviousVersion				''		model)
		-
		('more...' 								openMessageListMenu)).
	^ aMenu! !


!MessageSetWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:56:34'!
addExtraMenu2ItemsTo: aMenu
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		aMenu addList: #(
			-
			('remove from this browser'			removeMessageFromBrowser		''		model)
			('filter message list...'					filterMessageList))].
	aMenu add: 'sort by date' target: model action: #sortByDate! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:53:09'!
classListMenu
	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList: #(
		('browse full (b)'			browseMethodFull)
		('browse hierarchy (h)'		browseHierarchy)
		('browse protocol (p)'		browseFullProtocol)
		-
		('fileOut'					fileOutClass				''		model)
		-
		('show hierarchy'			hierarchy					''		model)
		('show definition'			editClass					''		model)
		('show comment'			editComment			''		model)
		-
		('inst var refs...'				browseInstVarRefs)
		('inst var defs...'			browseInstVarDefs)
		-
		('class var refs...'			browseClassVarRefs)
		('class vars'					browseClassVariables)
		('class refs (N)'				browseClassRefs)
		-
		('rename class ...'			renameClass				''		model)
		('copy class...'				copyClass				''		model)
		('remove class (x)'			removeClass				''		model)
		-
		('Run tests (t)'		runClassTests 	''	model)
		('more...'					offerClassListMenu2)).
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/5/2018 20:50:00'!
messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList:#(
			('what to show...'					offerWhatToShowMenu)
                	('toggle break on entry'		toggleBreakOnEntry						''		model)
            	-
			('browse full (b)' 					browseMethodFull)
			('browse hierarchy (h)'			browseHierarchy)
			('browse method (O)'			openSingleMessageBrowser)
			('browse protocol (p)'			browseFullProtocol)
			-
			('fileOut'							fileOutMessage							''		model)
			('explore CompiledMethod'		exploreCompiledMethod				''		model)
			-
			('senders of... (n)'				browseSendersOfMessages)
			('implementors of... (m)'		browseMessages)
			('inheritance (i)'					methodHierarchy)
			('versions (v)'						browseVersions)
			-
			('inst var refs...'					browseInstVarRefs)
			('inst var defs...'					browseInstVarDefs)
			('class var refs...'				browseClassVarRefs)
			('class variables'					browseClassVariables)
			('class refs (N)'					browseClassRefs)
			-
			('remove method (x)'			removeMessage							''		model)
			('Run test (t)'						runMethodTest 							'' 		model)
			('Debug test (r)'					debugMethodTest 						'' 		model)
			-
			('more...'							openMessageListMenu2)).
	^ aMenu
! !

!methodRemoval: MessageSetWindow #addExtraShiftedItemsTo:!
MessageSetWindow removeSelector: #addExtraShiftedItemsTo:!
!methodRemoval: BrowserWindow #addExtraShiftedItemsTo:!
BrowserWindow removeSelector: #addExtraShiftedItemsTo:!
!methodRemoval: BrowserWindow #shiftedClassListMenu!
BrowserWindow removeSelector: #shiftedClassListMenu!
!methodRemoval: BrowserWindow #shiftedMessageListMenu!
BrowserWindow removeSelector: #shiftedMessageListMenu!
!methodRemoval: CodeWindow #offerShiftedClassListMenu!
CodeWindow removeSelector: #offerShiftedClassListMenu!
!methodRemoval: CodeWindow #offerUnshiftedClassListMenu!
CodeWindow removeSelector: #offerUnshiftedClassListMenu!
!methodRemoval: CodeWindow #openShiftedMessageListMenu!
CodeWindow removeSelector: #openShiftedMessageListMenu!
