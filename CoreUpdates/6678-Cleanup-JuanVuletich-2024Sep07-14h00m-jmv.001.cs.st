'From Cuis7.1 [latest update: #6677] on 7 September 2024 at 2:13:37 pm'!

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 9/7/2024 14:00:11'!
triggerEvent: anEventSelector with: anObject

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector with: anObject ]! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/7/2024 14:13:12'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	doubleClickSelector ifNotNil: [
		^ (self window ifNil: [model]) perform: doubleClickSelector].
	selectedMorph ifNotNil: [
		self toggleExpandedState: selectedMorph.
		self scrollSelectionIntoView ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/7/2024 14:13:19'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index := self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ (self window ifNil: [model]) perform: doubleClickSelector! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/7/2024 14:10:32'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self rescaleButtons.
	model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:10:44'!
classAdded: addedClass

	model selectedSystemCategory = addedClass category 
		ifTrue: [ model changed: #classList ].! !

!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:10:51'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self canDiscardEdits ifTrue: [
		model classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName ].! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 9/7/2024 14:11:44'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (model baseCodeSource isLiveSmalltalkImage and: [ model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples ]
		ifFalse: [ "For non-standard browser configurations assume most of the default buttons are invalid"
			#(
				#(10 'show...' #offerWhatToShowMenu 'menu of what to show in lower pane' )
			) ].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/7/2024 14:11:07'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(40		'browse full (b)' 					$b 	window 			browseMethodFull 						editFindReplaceIcon)
				#(50		'class refs (N)' 					$N 	window 			browseClassRefs 						classIcon)
				#(60 		'-----')
				#(70		'fileIn' 					nil 	model 			fileInClass 						updateIcon)
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30 		'-----')
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			) ].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/7/2024 14:11:23'!
codeFileListMenuSpec
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10		'find class... (f)' 					$f 	window 			findClass)
				#(20 		'-----') ).
	model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30		'fileIn' 					nil 	model 			fileIn) )].
	answer := answer, 
			#(
				#(40		'fileOut' 					nil 	model 			fileOut) ).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50		'remove existing' 					nil 	model 			removeUnmodifiedClasses) )].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/7/2024 14:11:29'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileIn' 						nil 	model 			fileInMessage 							updateIcon)
				#(20		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(30 		'-----')
				#(40		'senders (n)' 						$n 	window 			browseSenders 							mailForwardIcon)
				#(50		'implementors (m)' 						$m 	window 			browseImplementors 							developmentIcon)
				#(60		'method inheritance (h)' 						$h 	window 			methodInheritance 							goDownIcon)
				#(70		'browse full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
				#(80		'versions (v)' 						nil 	window 			browseVersions 							clockIcon)
				#(90 		'-----')
				#(100		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(20 		'-----')
				#(30		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/7/2024 14:11:37'!
methodCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' 			false)
				#(10		'fileIn' 					nil 	model 			fileInMessageCategories 								updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' 			false)
			)].
	answer := answer, 
			#(
				#(20		'fileOut' 					nil 	model 			fileOutMessageCategories 								fileOutIcon)
				#(30 		'-----')
				#(40		'reorganize' 					nil 	model 			editMessageCategories 								sendReceiveIcon)
				#(50 		'-----')
				#(60		'add item...' 					nil 	model 			addCategory 								newIcon)
				#(70 		'-----')
				#(80		'rename...' 					nil 	model 			renameCategory 								saveAsIcon)
				#(90		'remove' 					nil 	model 			removeMessageCategory 								listRemoveIcon)
			).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 		'-----')
				#(100		'remove existing' 					nil 	model 			removeUnmodifiedMethods 								deleteIcon)
		) ].
	^answer! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/7/2024 14:11:51'!
fieldListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(10		'inspect (i)' 						$i 	window 			inspectSelection 							inspectIcon)
		#(20		'explore (I)' 						$I 	window 			exploreSelection 							exploreIcon)
		#(30		'copy to clipboard (c)' 						$c 	window 			copySelectionToClipboard 							copyIcon)
		#(40		'basic inspect' 						nil 	window 			inspectBasic 							inspectIcon)
		#(50		'references finder' 						nil 	window 			openReferencesFinder 							exploreIcon)
		#(60		'weight explorer' 						nil 	window 			openWeightExplorer 							exploreIcon)
	).
	(model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 		'-----')
		#(220		'browser full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
		#(230		'browser hierarch (h)' 						$h 	window 			browseHierarchy 							goTopIcon)
		#(240		'browser protocol (p)' 						$p 	window 			browseFullProtocol 							spreadsheetIcon)
	).
	^answer! !


!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'jmv 9/7/2024 14:12:07'!
objectsToSkipDuringRescan
	"Avoid observing side effects of the reference finder itself"

	| answer |
	answer := OrderedCollection
		with: self
		with: model
		with: model getCurrentSelection.
	self allMorphsDo: [:each | answer add: each].
	^answer.! !

!ObjectExplorerWindow methodsFor: 'menu commands - reference finder' stamp: 'jmv 9/7/2024 14:12:16'!
rescan

	| objectsToSkip |
	objectsToSkip := self objectsToSkipDuringRescan.
	model noteNewSelection: nil.
	model rescanSkipping: objectsToSkip.
	self expandAll.! !

