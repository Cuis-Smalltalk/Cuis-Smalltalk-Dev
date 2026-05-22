"
A view of the messages available to a class from itself upward through the class hierarchy. The viewed protocol can be pruned by selecting a superclass in the class hierarchy pane.

ProtocolBrowserWindow openFullProtocolForClass: Number.
"
Class {
	#name : 'ProtocolBrowserWindow',
	#superclass : 'MethodSetWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'as yet unclassified' }
ProtocolBrowserWindow class >> openFullProtocolForClass: aClass [
	"Create and schedule a browser for the entire protocol of the class."
	"
	ProtocolBrowserWindow openFullProtocolForClass: Number.
	"

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new on: aClass.
	label := 'Entire protocol of: ', aClass name.

	^self open: aPBrowser label: label
]

{ #category : 'GUI building' }
ProtocolBrowserWindow >> buildCategoriesList [
	"Define the message categories list pane"

	^ PluggableListMorph
		withModel: model
		listGetter: #categoriesList
		indexGetter: #categoriesListIndex
		indexSetter: #categoriesListIndex:
]

{ #category : 'GUI building' }
ProtocolBrowserWindow >> buildMorphicWindow [
	"Answer a morphic window that can display the receiver with a class hierarchy"
	| topRow |

	topRow := LinearLayoutMorph newRow.
	topRow 
		addMorph: self buildSelectiveClassList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildCategoriesList proportionalWidth: 0.25;
		addAdjusterMorph;
		addMorph: self buildMorphicMessageList proportionalWidth: 0.5.
	self layoutMorph
		addMorph: topRow proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection
]

{ #category : 'GUI building' }
ProtocolBrowserWindow >> buildSelectiveClassList [
	"Define the class hierarchy list pane"

	^(PluggableListMorph
		withModel: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:)
			menuNshortcutsKey: #classListMenuSpec.

]

{ #category : 'menu & kbd shortcuts' }
ProtocolBrowserWindow >> classListMenuSpec [
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'Browse Full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 	$h 	window 	browseHierarchy 		goTopIcon)
	)
]

{ #category : 'menu & kbd shortcuts' }
ProtocolBrowserWindow >> messageListMenuSpec [
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ super messageListMenuSpec, #(
		#(81		'Select Method Category (C)' 		$C 	model 		showHomeCategory 	editFindReplaceIcon)
		#(2050 		'XXX Remove from this Browser')
		#(2060 		'XXX Filter message list...'))
]
