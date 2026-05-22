"
Morphic view for ChangeSorter models. See category 'GUI building'.
"
Class {
	#name : 'ChangeSorterWindow',
	#superclass : 'CodeWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'services' }
ChangeSorterWindow class >> browseChangeSetsWithClass: class selector: selector [
	"Put up a menu comprising a list of change sets that hold changes for the given class and selector.  If the user selects one, open a single change-sorter onto it"

	| hits index |
	hits := ChangeSet allChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none].
	hits isEmpty ifTrue: [ ^ self inform: class name, '.', selector , '
is not in any change set'].
	index := hits size = 1
		ifTrue:	[1]
		ifFalse:	[
			(PopUpMenu
				labelArray: (hits collect: [ :cs | cs name ])
				lines: #()) 
					startUpMenu ].
	index = 0 ifTrue: [ ^ self ].
	self open: (ChangeSorter new myChangeSet: (hits at: index)) label: nil
]

{ #category : 'menu-world' }
ChangeSorterWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		20.
			#itemOrder 	-> 		20.
			#label 			-> 		'Change Sorter'.
			#object 		-> 		ChangeSorter.
			#selector 		-> 		#open.
			#icon 			-> 		#halfRefreshIcon.
			#balloonText 	-> 		'A tool allowing you to view the methods in a Change Set, especially changes to the Base System'.
		} asDictionary}`
]

{ #category : 'menu commands' }
ChangeSorterWindow >> browseMethodConflicts [
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |

	aList := model methodConflicts.
	aList isEmpty ifTrue: [
		^ self inform: 'No other change set has changes for any method in this change set.' ].

	MethodSetWindow
		open: (MethodSet messageList: aList)
		label: 'Methods in "', model changeSet name, '" that are also in other change sets (', aList size printString, ')'
]

{ #category : 'menu commands' }
ChangeSorterWindow >> browseVersions [
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	| class selector method category pair sourcePointer |

	(selector := model selectedMessageName) ifNil: [^ self].
	class := model selectedClassOrMetaClass.
	(class includesSelector: selector)
		ifTrue: [method := class compiledMethodAt: selector.
				category := class whichCategoryIncludesSelector: selector.
				sourcePointer := nil]
		ifFalse: [pair := model methodInfoFromRemoval: {class name. selector}.
				pair ifNil: [^ nil].
				sourcePointer := pair first.
				method := CompiledMethod toReturnSelf setSourcePointer: sourcePointer.
				category := pair last].
	VersionsBrowserWindow
		browseVersionsOf: method
		class: model selectedClass meta: class isMeta
		category: category selector: selector
		lostMethodPointer: sourcePointer
]

{ #category : 'GUI building' }
ChangeSorterWindow >> buildMorphicWindow [
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Theme current background.
	model myChangeSet ifNil: [
		"Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList := (PluggableListMorphByItem
				withModel: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:)
					menuNshortcutsKey: #changeSetListMenuSpec;
					autoDeselect: false.
	changeSetListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList := (PluggableListMorphByItem
				withModel: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:)
					menuNshortcutsKey: #classListMenuSpec.
	classListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList := (PluggableListMorphByItem
				withModel: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:)
					menuNshortcutsKey: #messageListMenuSpec.
	messageList := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString
]

{ #category : 'menu & kbd shortcuts' }
ChangeSorterWindow >> changeSetListMenuSpec [
	"Spec for the menu and shortcut keys for the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out, start a new cs (o)' 			$o 	model 		fileOutAndRemove 				fileOutIcon)
			#(20 		'File out, keep current cs (k)' 			$k 	model 		fileOutAndKeep 				fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 				$R 	model 		rename 						saveAsIcon)
			#(50 		'Destroy change set (x)' 			 	$x 	model 		remove 						warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Edit preamble (p)' 					$p 	model 		addPreamble 					textEditorIcon)
				#(80 	'Remove preamble' 					nil 	model 		removePreamble 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Add preamble (p)' 					$p 	model 		addPreamble 					listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 	'Edit postscript...' 					nil 	model 		editPostscript 					textEditorIcon)
				#(100 	'Remove postscript' 					nil 	model 		removePostscript 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 	'Add postscript...' 					nil 	model 		editPostscript 					listAddIcon) )].
	].

	answer  := answer, #(
		#(110 			'-----')
		#(120 			'Conflicts with other change sets' 		nil 	window		browseMethodConflicts 		emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 			'-----')
		#(140 			'Trim history' 						nil 	window		trimHistory 					clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 			'View affected class categories' 		nil 	model 		viewAffectedClassCategories	packageIcon
			'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 		'Rename change set (R)' 				$R 	model 		nil 								saveAsIcon)
			#(50 		'Destroy change set (x)' 		 		$x 	model 		nil 								warningIcon)) ].
	^answer
]

{ #category : 'menu & kbd shortcuts' }
ChangeSorterWindow >> classListMenuSpec [
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 	'Delete class from Change Set (d)' 	$d 	model 		forgetClass 			warningIcon)
			#(20 	'Remove class from System (x)' 	$x 	model 		removeClass 			deleteIcon)
			#(30 	'-----')
			#(40 	'Browse Full (b)' 					$b 	window 	browseMethodFull 		editFindReplaceIcon)
			#(50 	'Browse Hierarchy (h)' 			$h 	window 	browseHierarchy 		goTopIcon)
			#(60 	'Browse Protocol (p)' 				$p 	window 	browseFullProtocol 	spreadsheetIcon)
			#(70 	'-----')		
			#(80 	'Class References (N)' 			$N 	window 	browseClassRefs 		classIcon)
	).

]

{ #category : 'GUI building' }
ChangeSorterWindow >> initialExtent [
	^`540@300` * (Preferences at: #standardCodeFont) lineSpacing // 14
]

{ #category : 'menu & kbd shortcuts' }
ChangeSorterWindow >> messageListMenuSpec [
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 	'Delete method from Change Set (d)' 	$d 	model 		forget 								warningIcon)
			#(20 	'Remove method from System (x)' 	$x 	model 		removeMessage 					deleteIcon)
			#(30 	'-----')
			#(40 	'Browse Full (b)' 						$b 	window 	browseMethodFull 					editFindReplaceIcon)
			#(50 	'Browse Hierarchy (h)' 				$h 	window 	browseHierarchy 					goTopIcon)
			#(60 	'Browse Method (O)' 					$O 	window 	openSingleMessageBrowser 		scriptIcon)
			#(80 	'-----')
			#(90 	'File Out (o)' 							$o 	model 		fileOutMessage 					fileOutIcon)
			#(100 	'-----')		
			#(110	'Senders of... (n)' 					nil 	window 	browseSendersOfMessages 		mailForwardIcon)
			#(120	'Implementors of... (m)' 				nil 	window 	browseImplementorsOfMessages 	developmentIcon)
			#(130 	'Inheritance (i)' 						$i 	window 	methodInheritance 				goDownIcon)
			#(140 	'Versions (v)' 						$v 	window 	browseVersions 					clockIcon)

			"Keyboard shortcut only. No menu entry."
			#(991	nil 									$m 	window 	browseImplementors)
			#(992	nil 									$n 	window 	browseSenders)
	).

]

{ #category : 'menu commands' }
ChangeSorterWindow >> trimHistory [
	
	(self confirm: self trimHistoryConfirmationMessage)
		ifFalse: [ ^ self ].
	
	model trimHistory
	
	
]

{ #category : 'menu commands' }
ChangeSorterWindow >> trimHistoryConfirmationMessage [
	
	^ String streamContents: [ :confirmationMessage |
		confirmationMessage
			nextPutAll: 'You are about to permanently remove non-essential history,';
			newLine;
			nextPutAll: 'including methods that were added and then removed,';
			newLine;
			nextPutAll: 'as well as renames and reorganizations of newly added classes.';
			newLine;
			newLine;
			nextPutAll: 'This action cannot be undone.';
			newLine;
			newLine;
			nextPutAll: 'Do you want to continue?'
		]
]

{ #category : 'GUI building' }
ChangeSorterWindow >> windowColor [
	^ Theme current changeSorter
]
