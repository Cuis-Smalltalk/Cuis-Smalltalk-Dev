'From Cuis7.1 [latest update: #6436] on 4 June 2024 at 12:27:06 pm'!

!MessageNamesWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 12:22:14'!
selectorListMenuSpec
	"Spec for the menu and shortcut keys for the selectors list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Selectors' 					false)
		#(10		'senders (n)' 						$n 	window 			browseSenders 						mailForwardIcon)
		#(20		'implementors of... (m)' 						$m 	window 			browseImplementors 						developmentIcon)
	).
! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 12:25:59'!
buildMorphicWindow
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
	firstRow := LayoutMorph newRow.
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
	secondRow := LayoutMorph newRow.
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
	model changed: #editSelection! !

!methodRemoval: MessageNamesWindow #selectorListMenu stamp: 'jmv 6/4/2024 12:25:41'!
MessageNamesWindow removeSelector: #selectorListMenu!
!methodRemoval: MessageNamesWindow #selectorListKey:from: stamp: 'jmv 6/4/2024 12:25:44'!
MessageNamesWindow removeSelector: #selectorListKey:from:!

!MessageNamesWindow reorganize!
('GUI building' buildMorphicWindow submorphToFocusKeyboard windowColor)
('menu & kbd shortcuts' selectorListMenuSpec)
!

