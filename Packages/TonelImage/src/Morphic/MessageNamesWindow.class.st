"
Morphic view for MessageNames models. See category 'GUI building'.
"
Class {
	#name : 'MessageNamesWindow',
	#superclass : 'MethodSetWindow',
	#instVars : [
		'textMorph'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu-world' }
MessageNamesWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		40.
			#label 			-> 		'Message Names'.
			#object 		-> 		MessageNames.
			#selector 		-> 		#open.
			#icon 			-> 		#inspectIcon.
			#balloonText 	-> 		'A tool for finding and editing methods that contain any given keyword in their names.'.
		} asDictionary}`
]

{ #category : 'GUI building' }
MessageNamesWindow >> buildMorphicWindow [
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames open"
	| selectorListView firstRow searchButton secondRow |
	textMorph := (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton := PluggableButtonMorph new
		model: textMorph innerTextMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow := LinearLayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView := (PluggableListMorph
		withModel: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:)
			menuNshortcutsKey: #selectorListMenuSpec.
	secondRow := LinearLayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection
]

{ #category : 'menu & kbd shortcuts' }
MessageNamesWindow >> selectorListMenuSpec [
	"Spec for the menu and shortcut keys for the selectors list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Selectors' 					false)
		#(10	'Senders (n)' 				$n 	window 	browseSenders 			mailForwardIcon)
		#(20	'Implementors of... (m)' 		$m 	window 	browseImplementors 		developmentIcon)
	).

]

{ #category : 'GUI building' }
MessageNamesWindow >> submorphToFocusKeyboard [
	^textMorph innerTextMorph
]

{ #category : 'GUI building' }
MessageNamesWindow >> windowColor [
	^ Theme current messageNames
]
