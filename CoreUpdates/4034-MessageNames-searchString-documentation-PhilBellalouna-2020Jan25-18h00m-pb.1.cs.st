'From Cuis 5.0 [latest update: #4030] on 25 January 2020 at 6:29:18 pm'!

!MessageNames commentStamp: '<historical>' prior: 0!
Search for message names.  There are several special characters that alter how searchString is interpreted:
$; - separate several search criteria (like 'editorClassFor:;contentsSelection')
$* - matches a string pattern rather than just a simple string match (i.e. 'set*text')
$# - matches a single character (for example, 'ini###lize'!


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'pb 1/25/2020 18:22:25'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames openMessageNames"
	| selectorListView firstRow searchButton secondRow |
	textMorph _ (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton _ PluggableButtonMorph new
		model: textMorph textMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow _ LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView _ PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow _ LayoutMorph newRow.
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
	model changed: #editSelection.! !

