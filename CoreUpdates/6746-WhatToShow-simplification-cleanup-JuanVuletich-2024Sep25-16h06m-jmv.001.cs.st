'From Cuis7.1 [latest update: #6748] on 25 September 2024 at 4:13:35 pm'!

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:02:54'!
showByteCodes
	self contentsSymbol: #byteCodes! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:37:03'!
showDecompile
	self contentsSymbol: #decompile! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:05:46'!
showLineDiffs
	self contentsSymbol: #lineDiffs! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:33:39'!
showPrettyLineDiffs
	self contentsSymbol: #prettyLineDiffs! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:04:47'!
showPrettyPrint
	self contentsSymbol: #prettyPrint! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:33:49'!
showPrettyWordDiffs
	self contentsSymbol: #prettyWordDiffs! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 12:58:48'!
showSource
	self contentsSymbol: #source! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:34:00'!
showWordDiffs
	self contentsSymbol: #wordDiffs! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:25:11'!
showingPrettyPrintString
	"Answer whether the receiver is showing pretty-print"

	^ self showingPrettyPrint asMenuItemTextPrefix,
		'prettyPrint'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 9/24/2024 13:24:22'!
showingSourceString
	"Answer a string telling whether the receiver is showing plain source"

	^ self showingSource asMenuItemTextPrefix,
		'source'! !


!CodeProvider methodsFor: 'contents' stamp: 'jmv 9/24/2024 13:53:19'!
acceptedStringOrText
	"Answer the source code or documentation for the selected method"

	self showingByteCodes ifTrue: [
		^ self selectedBytecodes].

	^ self selectedMessage! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/25/2024 16:01:04'!
toggleLineDiffing
	"Toggle whether regular-diffing should be shown in the code pane"

	self showingLineDiffs
		ifFalse: [self showLineDiffs]
		ifTrue: [self showSource]! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/25/2024 16:00:39'!
togglePrettyLineDiffing
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showingPrettyLineDiffs
		ifFalse: [self showPrettyLineDiffs]
		ifTrue: [self showSource]! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/25/2024 16:00:53'!
togglePrettyWordDiffing
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showingPrettyWordDiffs
		ifFalse: [self showPrettyWordDiffs]
		ifTrue: [self showSource]! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 9/25/2024 16:01:21'!
toggleWordDiffing
	"Toggle whether regular-diffing should be shown in the code pane"

	self showingWordDiffs
		ifFalse: [self showWordDiffs]
		ifTrue: [self showSource]! !

!CodeProvider methodsFor: 'misc' stamp: 'jmv 9/24/2024 13:53:25'!
okayToAccept
	"Answer whether it is okay to accept the receiver's input"

	self showingByteCodes ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingAnyKindOfDiffs ifTrue: [
		^ SelectionMenu confirm: 
'Caution!!  You are "showing diffs" here, so 
there is a danger that some of the text in the
code pane is contaminated by the "diff" display'
		trueChoice: 'accept anyway -- I''ll take my chances' falseChoice: 'um, let me reconsider' icons: #(acceptIcon cancelIcon)
	].

	^ true! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 9/24/2024 13:54:35'!
selectedMessage
	"Answer a copy of the source code for the selected message.  This generic version is probably actually never reached, since every subclass probably reimplements and does not send to super.  In time, ideally, most, or all, reimplementors would vanish and all would defer instead to a universal version right here.  Everything in good time."

	| class selector method |

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class := self selectedClassOrMetaClass.
	(class isNil or: [(selector := self selectedMessageName) isNil]) ifTrue: [^ ''].
	method := class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod := method.

	^ self sourceStringPrettifiedAndDiffed
			copy! !


!Browser methodsFor: 'message list' stamp: 'jmv 9/24/2024 13:53:37'!
selectedMessage
	"Answer a copy of the source code for the selected message."

	| class selector method |
	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	method := class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod := method.

	^ self sourceStringPrettifiedAndDiffed
			copy! !


!MethodSet methodsFor: 'message list' stamp: 'jmv 9/24/2024 13:53:46'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization commentRemoteStr.
			^ class comment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.

	^self sourceStringPrettifiedAndDiffed! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 9/24/2024 13:16:21'!
optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph
			model: self
			action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		tuple size > 3 ifTrue: [ button setBalloonText: tuple fourth ].
		tuple size > 4 ifTrue: [ button secondaryActionSelector: tuple fifth ].
		buttons add: button ].
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row
		addMorphs: buttons
		widthProportionalTo: widths.
	^ row.! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 13:57:21'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 									preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 									debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(70 		'-----')
		#(80		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(90		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(100		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(110		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(120 		'-----')
		#(130		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(140		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(150		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 									developmentIcon)
		#(160		'inheritance (i)' 							$i 	window 			methodInheritance 									goDownIcon)
		#(170		'versions (v)' 							$v 	window 			browseVersions 									clockIcon)
		#(180 		'-----')
		#(190		'remove method (x)' 							$x 	model 			removeMessage 									deleteIcon)
		#(200		'remove method in hierarchy' 							nil 	model 			removeMessageInHierarchy 									deleteIcon)
		#(210		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)

		" 'refactorings' SubMenu"
		#(1010		'rename... (R)' 											$R 	model 			renameSelector 											saveAsIcon)
		#(1020		'change keyword order...' 											nil 	model 			changeKeywordOrder 											sendReceiveIcon)
		#(1030		'add parameter... (A)' 											$A 	model 			addParameter 											listAddIcon)
		#(1040		'remove parameter... (S)' 											$S 	model 			removeParameter 											listRemoveIcon)
		#(1050 		'-----')
		#(1060		'inline method... (F)' 											$F 	model 			inlineMethod 											saveAsIcon)
		#(1070		'move to instance/class method' 											nil 	model 			moveToInstanceOrClassMethod 											changesIcon)
		#(1080		'push up' 											nil 	model 			pushUpSelector 											goTopIcon)
		#(1090		'add in superclass as subclassResponsability' 											nil 	model 			addAsSubclassResponsabilitySelector 											goTopIcon)
		#(1100		'push down to subclasses' 											nil 	model 			pushDownSelectorToSubclasses 											goBottomIcon)
		#(1110		'push down to one subclass' 											nil 	model 			pushDownSelectorToOneSubclass 											goBottomIcon)

		" 'more' SubMenu"
		#(2010		'local senders of...' 								nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(2020		'local implementors of...' 								nil 	window 			browseLocalImplementorsOfMessages 											classIcon)
		#(2030 		'-----')
		#(2040		'sample instance' 								nil 	window 			makeSampleInstance 											instanceIcon)
		#(2050		'inspect instances' 								nil 	window 			inspectInstances 											inspectIcon)
		#(2060		'inspect subinstances' 								nil 	window 			inspectSubInstances 											inspectIcon)
		#(2070 		'-----')
		#(2080		'change category...' 								nil 	model 			changeCategory 											saveAsIcon)
		#(2090		'select method category (C)' 								$C 	model 			showHomeCategory 											editFindReplaceIcon)
		#(2100		'change sets with this method' 								nil 	window 			findMethodInChangeSets 											changesIcon)
		#(2110		'revert to previous version' 								nil 	model 			revertToPreviousVersion 											undoIcon)
	)! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 9/25/2024 15:47:50'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph 
					model: model
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 13:59:38'!
listMenuSpec
	"Spec for the menu and shortcut keys for the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10		'fileIn selections' 									nil 	model 			fileInSelections 										nil
					'import the selected items into the image')
		#(20		'fileOut selections...	' 								nil 	model 			fileOutSelections 										nil
		 			'create a new file containing the selected items')
		#(30		'fileOut current version of selections...' 					nil 					model 			fileOutCurrentVersionsOfSelections 										nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 		'-----')
		#(50		'compare to current' 									nil 	model 			compareToCurrentVersion 										nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60 		'-----')
		#(70		'select new methods' 									nil 	model 			selectNewMethods 										nil
 					'select methods in the file that do not currently exist in the image')
		#(80		'select changes for absent classes' 									nil 	model 			selectAllForAbsentClasses 										nil
					'select methods in the file for classes that are not defined in the image')
		#(90		'select all changes for this class' 									nil 	model 			selectAllForThisClass 										nil
					'select all methods in the file that belong to the currently-selected class')
		#(100		'select unchanged methods' 									nil 	model 			selectUnchangedMethods 										nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(110		'select methods equivalent to current' 									nil 	model 			selectEquivalentMethods 										nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(120		'select methods older than current' 									nil 	model 			selectMethodsOlderThanCurrent 										nil
					'select methods in the file that are older than the one currently in the image')
		#(130		'select removals of sent methods' 									nil 	model 			selectRemovalsOfSent 										nil
					'select all method removals of methods that have some sender in the image')
		#(140 		'-----')
		#(150		'select all (a)' 									$a 	model 			selectAll 										nil
					'select all the items in the list')
		#(160		'deselect all (A)' 									$A 	model 			deselectAll 										nil
					'deselect all the items in the list')
		#(170		'invert selections' 									nil 	model 			invertSelections 										nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(180 		'-----')
		#(190		'browse class and method' 									nil 	window 			browseMethodFull 										nil
					'open a full browser showing the selected method')
		#(200		'browse all versions of single selection' 									nil 	window 			browseVersions 										nil
					'open a version browser showing the versions of the currently selected method')
		#(210		'browse current versions of selections' 									nil 	window 			browseCurrentVersionsOfSelections 										nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(220		'destroy current methods of selections' 									nil 	model 			destroyCurrentCodeOfSelections 										nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(230 		'-----')
		#(240		'remove doIts' 									nil 	model 			removeDoIts 										nil
					'remove all items that are doIts rather than definitions')
		#(250		'remove older versions' 									nil 	model 			removeOlderMethodVersions 										nil
					'remove all but the most recent versions of methods in the list')
		#(260		'remove up-to-date versions' 									nil 	model 			removeUpToDate 										nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(270		'remove empty class comments' 									nil 	model 			removeEmptyClassComments 										nil
					'remove all empty class comments')
		#(280		'remove selected items' 									nil 	model 			removeSelections 										nil
					'remove the selected items from the change-list')
		#(290		'remove unselected items' 									nil 	model 			removeNonSelections 										nil
					'remove all the items not currently selected from the change-list')
	)! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 14:00:02'!
classCommentVersionsMenuSpec
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Versions' 							true)
		#(10		'compare to current' 							nil 	model 			compareToCurrentVersion 										nil
					'compare selected version to the current version')
		#(20		'revert to selected version' 							nil 	model 			fileInSelections 										nil
					'resubmit the selected version, so that it becomes the current version')
		#(30 		'-----')
		#(40		'update list' 							nil 	model 			reformulateList 										nil
					'reformulate the list of versions, in case it somehow got out of synch with reality')
		#(50 		'-----')
		#(60		'help...' 							nil 	model 			offerVersionsHelp 										nil
					'provide an explanation of the use of this tool')
	)! !

!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 14:00:20'!
methodVersionsMenuSpec
	"Spec for the menu and shortcut keys for a method version in the versions list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
				#(0 		'Versions' 									true)
	).
	model listIndex > 0 ifTrue: [
		(model list size > 1 ) ifTrue: [
			answer := answer, #(
				#(10		'compare to current' 									nil 	model 			compareToCurrentVersion 									nil
							'compare selected version to the current version')
				#(20		'compare to version...' 									nil 	model 			compareToOtherVersion 									nil
							'compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'revert to selected version (z)' 									$z 	model 			fileInSelections 									nil
							'resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'edit current method (O)' 									$O 	window 			openSingleMessageBrowser 									nil
							'open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'update list' 									nil 	model 			reformulateList 									nil
							'reformulate the list of versions, in case it somehow got out of synch with reality')
				#(70 		'-----')
				#(80		'senders (n)' 									$n 	window 			browseSenders 									nil
							'browse all senders of this selector')
				#(90		'implementors (m)' 									$m 	window 			browseImplementors 									nil
							'browse all implementors of this selector')
				#(100 		'-----')
				#(110		'help...' 									nil 	model 			offerVersionsHelp 									nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 9/24/2024 13:17:09'!
customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons := OrderedCollection new.
	buttonColor := self buttonColor.
	"button with target = self"
	button := PluggableButtonMorph 
		model: self
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button := PluggableButtonMorph 
					model: self
					action: tuple second.
		button color: buttonColor.
		button label: tuple first.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 14:00:57'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 											preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 											debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 											editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 											goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 											scriptIcon)
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 											spreadsheetIcon)
		#(80 		'-----')
		#(90		'fileOut' 							nil 	model 			fileOutMessage 											fileOutIcon)
		#(100		'run test (t)' 							$t 	model 			runMethodTest 											weatherFewCloudsIcon)
		#(110		'debug test (r)' 							$r 	model 			debugMethodTest 											weatherFewCloudsIcon)
		#(120		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 											exploreIcon)
		#(130 		'-----')
		#(140		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 											mailForwardIcon)
		#(150		'local senders of...' 							nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(160		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 											developmentIcon)
		#(170		'local implementors of...' 							nil 	window 			browseLocalImplementorsOfMessages 											developmentIcon)
		#(180		'inheritance (i)' 							$i 	window 			methodInheritance 											goDownIcon)
		#(190		'versions (v)' 							$v 	window 			browseVersions 											clockIcon)
		#(200 		'-----')
		#(210		'remove method (x)' 							$x 	model 			removeMessage 											deleteIcon)
		#(220		'more' 							nil 	nil 			2000 											listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)
		#(994		nil 							$N 	window 			browseClassRefs)

		" 'more' SubMenu"
		#(2010		'sample instance' 								nil 	window 			makeSampleInstance 										instanceIcon)
		#(2020		'inspect instances' 								nil 	window 			inspectInstances 										inspectIcon)
		#(2030		'inspect subinstances' 								nil 	window 			inspectSubInstances 										inspectIcon)
		#(2040 		'-----')
		#(2050 		'remove from this browser' 								nil 	model 			removeMessageFromBrowser 										listRemoveIcon)
		#(2060 		'filter message list...' 								nil 	window 			filterMessageList 										findIcon)
		#(2070		'change category...' 								nil 	model 			changeCategory 										saveAsIcon)
		#(2080		'change sets with this method' 								nil 	window 			findMethodInChangeSets 										changesIcon)
		#(2090		'revert to previous version' 								nil 	model 			revertToPreviousVersion 										undoIcon)
		#(2100 		'-----')
		#(2110 		'sort by class hierarchy' 								nil 	model 			sortByClassHierarchy 										classIcon)
		#(2120 		'sort by class name' 								nil 	model 			sortByClassName 										classIcon)
		#(2130 		'sort by selector' 								nil 	model 			sortBySelector 										scriptIcon)
		#(2140 		'sort by date' 								nil 	model 			sortByDate 										dateIcon)
		#(2150 		'reverse sort' 								nil 	model 			sortReverse 										redoIcon)
	).
! !


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 14:18:08'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ super messageListMenuSpec, #(
		#(81		'select method category (C)' 							$C 	model 			showHomeCategory 						editFindReplaceIcon)
		#(2050 		'XXremove from this browser')
		#(2060 		'XXfilter message list...'))! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 9/24/2024 13:17:13'!
buttonToTriggerIn: aFileList for: service
	"Answer a button that will trigger the receiver service in a file list"

	| aButton |
	service argumentProvider: aFileList.
	aButton := PluggableButtonMorph 
				model: service
				action: #performService.
	aButton label: service buttonLabel.
	aButton color: self buttonColor.
	aButton setBalloonText: service description.
	^aButton! !

!methodRemoval: ChangeSorter #toggleDiffing stamp: 'jmv 9/25/2024 16:09:17'!
ChangeSorter removeSelector: #toggleDiffing!
!methodRemoval: CodeProvider #toggleDecompile stamp: 'jmv 9/25/2024 16:08:28'!
CodeProvider removeSelector: #toggleDecompile!
!methodRemoval: CodeProvider #showingDocumentationString stamp: 'jmv 9/25/2024 16:08:30'!
CodeProvider removeSelector: #showingDocumentationString!
!methodRemoval: CodeProvider #showDecompile: stamp: 'jmv 9/25/2024 16:08:30'!
CodeProvider removeSelector: #showDecompile:!
!methodRemoval: CodeProvider #prettyPrintString stamp: 'jmv 9/25/2024 16:08:31'!
CodeProvider removeSelector: #prettyPrintString!
!methodRemoval: CodeProvider #showWordDiffs: stamp: 'jmv 9/25/2024 16:08:32'!
CodeProvider removeSelector: #showWordDiffs:!
!methodRemoval: CodeProvider #showPrettyWordDiffs: stamp: 'jmv 9/25/2024 16:08:33'!
CodeProvider removeSelector: #showPrettyWordDiffs:!
!methodRemoval: CodeProvider #showComment stamp: 'jmv 9/25/2024 16:08:34'!
CodeProvider removeSelector: #showComment!
!methodRemoval: CodeProvider #showingDocumentation stamp: 'jmv 9/25/2024 16:09:01'!
CodeProvider removeSelector: #showingDocumentation!
!methodRemoval: CodeProvider #showingPlainSourceString stamp: 'jmv 9/25/2024 16:08:38'!
CodeProvider removeSelector: #showingPlainSourceString!
!methodRemoval: CodeProvider #togglePrettyPrint stamp: 'jmv 9/25/2024 16:08:39'!
CodeProvider removeSelector: #togglePrettyPrint!
!methodRemoval: CodeProvider #showDocumentation: stamp: 'jmv 9/25/2024 16:08:57'!
CodeProvider removeSelector: #showDocumentation:!
!methodRemoval: CodeProvider #togglePlainSource stamp: 'jmv 9/25/2024 16:08:41'!
CodeProvider removeSelector: #togglePlainSource!
!methodRemoval: CodeProvider #commentContents stamp: 'jmv 9/25/2024 16:08:43'!
CodeProvider removeSelector: #commentContents!
!methodRemoval: CodeProvider #showByteCodes: stamp: 'jmv 9/25/2024 16:08:54'!
CodeProvider removeSelector: #showByteCodes:!
!methodRemoval: CodeProvider #showDiffs: stamp: 'jmv 9/25/2024 16:09:26'!
CodeProvider removeSelector: #showDiffs:!
!methodRemoval: CodeProvider #showLineDiffs: stamp: 'jmv 9/25/2024 16:08:46'!
CodeProvider removeSelector: #showLineDiffs:!
!methodRemoval: CodeProvider #toggleShowDocumentation stamp: 'jmv 9/25/2024 16:08:51'!
CodeProvider removeSelector: #toggleShowDocumentation!
!methodRemoval: CodeProvider #toggleDiffing stamp: 'jmv 9/25/2024 16:09:17'!
CodeProvider removeSelector: #toggleDiffing!
!methodRemoval: CodeProvider #showingPlainSource stamp: 'jmv 9/25/2024 16:08:49'!
CodeProvider removeSelector: #showingPlainSource!
!methodRemoval: CodeProvider #toggleShowingByteCodes stamp: 'jmv 9/25/2024 16:08:48'!
CodeProvider removeSelector: #toggleShowingByteCodes!
!methodRemoval: CodeProvider #showPrettyLineDiffs: stamp: 'jmv 9/25/2024 16:08:47'!
CodeProvider removeSelector: #showPrettyLineDiffs:!
