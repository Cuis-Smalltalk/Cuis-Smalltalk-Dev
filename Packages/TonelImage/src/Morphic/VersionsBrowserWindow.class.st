"
Morphic view for VersionsBrowser models. See category 'GUI building'.
"
Class {
	#name : 'VersionsBrowserWindow',
	#superclass : 'ChangeListWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'services' }
VersionsBrowserWindow class >> browseCommentOf: class [

	| changeList |
	changeList := ClassCommentVersionsBrowser new scanVersionsOf: class.
	changeList ifNil: [^ self inform: 'No versions available'].
	self
		open: changeList
		label: 'Recent versions of ', class name, '''s comments'
]

{ #category : 'services' }
VersionsBrowserWindow class >> browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector  [
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil
]

{ #category : 'services' }
VersionsBrowserWindow class >> browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChangeRecord: aChangeRecord [
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChangeRecord: aChangeRecord
]

{ #category : 'services' }
VersionsBrowserWindow class >> browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer  [
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChangeRecord: nil
]

{ #category : 'services' }
VersionsBrowserWindow class >> browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer extraChangeRecord: aChangeRecord [
	| browser |
	browser := VersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	aChangeRecord ifNotNil: [browser addedChangeRecord: aChangeRecord ].
	browser toggleListIndex: 1.
	VersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> buildMorphicWindow [
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorph
		withModel: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
]

{ #category : 'menu & kbd shortcuts' }
VersionsBrowserWindow >> classCommentVersionsMenuSpec [
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Versions' 					true)
		#(10	'Compare to Current' 			nil 	model 	compareToCurrentVersion 	nil 	'compare selected version to the current version')
		#(20	'Revert to selected Version' 	nil 	model 	fileInSelections 			nil 	'resubmit the selected version, so that it becomes the current version')
		#(30 	'-----')
		#(40	'Update list' 					nil 	model 	reformulateList 			nil 	'reformulate the list of versions, in case it somehow got out of synch with reality')
		#(50 	'-----')
		#(60	'Help...' 						nil 	model 	offerVersionsHelp 			nil 	'provide an explanation of the use of this tool')
	)
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> lineDiffButtonHelp [
	^'Show code differences from the previous version, line by line.'
]

{ #category : 'menu & kbd shortcuts' }
VersionsBrowserWindow >> listMenuSpec [
	"Spec for the menu and shortcut keys for the versions list."
	^model classCommentIndicated
		ifTrue: [ self classCommentVersionsMenuSpec ]
		ifFalse: [ self methodVersionsMenuSpec ]
]

{ #category : 'menu & kbd shortcuts' }
VersionsBrowserWindow >> methodVersionsMenuSpec [
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
				#(10		'Compare to current' 									nil 	model 		compareToCurrentVersion 		nil
							'Compare selected version to the current version')
				#(20		'Compare to version...' 								nil 	model 		compareToOtherVersion 		nil
							'Compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'Revert to selected version (z)' 						$z 	model 		fileInSelections 				nil
							'Resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'Edit current method (O)' 								$O 	window 	openSingleMessageBrowser 	nil
							'Open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'Update List' 											nil 	model 		reformulateList 				nil
							'Reformulate the list of versions, in case it somehow got out of synch with reality')
				#(70 		'-----')
				#(80		'Senders (n)' 										$n 	window 	browseSenders 				nil
							'Browse all Senders of this selector')
				#(90		'Implementors (m)' 									$m 	window 	browseImplementors 			nil
							'Browse all Implementors of this selector')
				#(100 		'-----')
				#(110		'Help...' 												nil 	model 		offerVersionsHelp 				nil
							'provide an explanation of the use of this tool')
	).
	^answer
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> optionalModelButtonTuples [

	^#(
		(25
		'compare to current'
		compareToCurrentVersion
		'opens a separate window which shows the text differences between the selected version and the current version')

		(10
		'revert'
		fileInSelections
		'reverts the method to the version selected')

		(7
		'help'
		offerVersionsHelp
		'further explanation about use of Versions browsers')
	)
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> prettyLineDiffButtonHelp [
	^'Show pretty-printed code differences from the previous version, line by line.'
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> prettyWordDiffButtonHelp [
	^'Show pretty-printed code differences from the previous version, word by word.'
]

{ #category : 'updating' }
VersionsBrowserWindow >> updateListsAndCodeNow [
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> windowColor [
	^ Theme current versionsBrowser
]

{ #category : 'GUI building' }
VersionsBrowserWindow >> wordDiffButtonHelp [
	^'Show code differences from the previous version, word by word.'
]
