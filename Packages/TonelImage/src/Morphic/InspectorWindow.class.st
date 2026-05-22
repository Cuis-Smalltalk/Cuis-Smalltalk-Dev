"
A Window built specifically to be the view on an Inspector model object. See category 'GUI building'.
"
Class {
	#name : 'InspectorWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu commands' }
InspectorWindow >> addEntry [
	self request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		do: [:aString|
			model addEntry: (Compiler evaluate: aString)]
]

{ #category : 'menu commands' }
InspectorWindow >> browseFullProtocol [
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClassOrMetaClass
]

{ #category : 'menu commands' }
InspectorWindow >> browseHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	Smalltalk
		browseHierarchy: model selectedClassOrMetaClass
		selector: nil.
]

{ #category : 'menu commands' }
InspectorWindow >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass := model selectedClassOrMetaClass) ifNotNil: [
		Smalltalk browseFull: myClass selector: nil ].
]

{ #category : 'GUI building' }
InspectorWindow >> buildContentsText [

	| contentsText |

	contentsText := TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
			
	^contentsText
]

{ #category : 'GUI building' }
InspectorWindow >> buildEvaluatorText [

	| evaluatorText |
	
	evaluatorText := (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression'.
	
	^evaluatorText 
]

{ #category : 'GUI building' }
InspectorWindow >> buildList [

	| list |
	
	list := (PluggableListMorph
			withModel: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:)
				menuNshortcutsKey: #fieldListMenuSpec.
	list doubleClickSelector: #inspectSelection.
	^list
]

{ #category : 'GUI building' }
InspectorWindow >> buildMorphicWindow [
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |

	"Build widgets. We'll assemble them below."
	list := self buildList.
	contentsText := self buildContentsText.
	evaluatorText := self buildEvaluatorText.
	
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow := LinearLayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	self setLabel: model displayLabel
]

{ #category : 'menu commands' }
InspectorWindow >> copySelectionToClipboard [
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model selection
]

{ #category : 'menu commands' }
InspectorWindow >> exploreSelection [
	"Create and schedule an Explorer on the receiver's model's currently selected object."

	^ model selection explore
]

{ #category : 'menu & kbd shortcuts' }
InspectorWindow >> fieldListMenuSpec [
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(10	'Inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
		#(20	'Explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
		#(30	'Copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
		#(40	'Basic Inspect' 			nil 	window 	inspectBasic 				inspectIcon)
		#(50	'References finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
		#(60	'Weight explorer' 		nil 	window 	openWeightExplorer 		exploreIcon)
	).
	(model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 	'-----')
		#(220	'Browse Full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
		#(230	'Browse Hierarch (h)' 		$h 	window 	browseHierarchy 			goTopIcon)
		#(240	'Browse Protocol (p)' 		$p 	window 	browseFullProtocol 		spreadsheetIcon)
	).
	^answer
]

{ #category : 'GUI building' }
InspectorWindow >> initialExtent [

	^`600@325` * (Preferences at: #standardCodeFont) lineSpacing // 14
]

{ #category : 'menu commands' }
InspectorWindow >> inspectBasic [
	"Create and schedule a basic Inspector on the receiver's model's currently selected object."

	^ model selection basicInspect
]

{ #category : 'menu commands' }
InspectorWindow >> inspectSelectedKey [

	^ model selectedKey inspect
]

{ #category : 'menu commands' }
InspectorWindow >> inspectSelection [
	"Create and schedule an Inspector on the receiver's model's currently selected object."

	^ model selection inspect
]

{ #category : 'menu & kbd shortcuts' }
InspectorWindow >> menuSpecForDictionary [
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 		'-----')
		#(110		'Inspect Key' 		nil 	window 	inspectSelectedKey 		editFindReplaceIcon)
		#(120		'Senders of this Key' 	nil 	window 	sendersOfSelectedKey 		mailForwardIcon)
		#(130		'Add Key' 			nil 	window 	addEntry 					listAddIcon)
		#(140		'Rename Key' 		nil 	window 	renameEntry 				saveAsIcon)
		#(150		'Remove' 			nil 	model 		removeSelection 			listRemoveIcon)
	)
]

{ #category : 'menu & kbd shortcuts' }
InspectorWindow >> menuSpecForSet [
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 	'-----')
		#(110	'Remove' 		nil 	model 		removeSelection 		listRemoveIcon)
	)
]

{ #category : 'menu commands' }
InspectorWindow >> openReferencesFinder [
	"Create and schedule a References Finder on the receiver's model's currently selected object."

	^ReferenceFinder openExplorerOn: model selection
]

{ #category : 'menu commands' }
InspectorWindow >> openWeightExplorer [
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model selection
]

{ #category : 'menu commands' }
InspectorWindow >> renameEntry [
	self request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		initialAnswer: model selectedKey printString
		do: [:aString| aString isEmpty ifFalse: [model renameEntryTo: (Compiler evaluate: aString)]]
]

{ #category : 'menu commands' }
InspectorWindow >> sendersOfSelectedKey [
	| key |
	key := model selectedKey.
	key isString ifFalse: [ ^self ].
	Smalltalk browseAllCallsOn: key
]
