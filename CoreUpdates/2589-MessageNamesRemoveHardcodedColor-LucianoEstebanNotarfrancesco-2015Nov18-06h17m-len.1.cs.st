'From Cuis 4.2 of 25 July 2013 [latest update: #2583] on 18 November 2015 at 7:11:18.458198 am'!

!MessageNamesWindow methodsFor: 'GUI building' stamp: 'len 11/18/2015 07:09'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection.
	textMorph textMorph setProperty: #alwaysAccept toValue: true.
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
		addMorph: searchButton proportionalWidth: 0.25;
		addMorph: textMorph proportionalWidth: 0.75.

	selectorListView _ PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow _  LayoutMorph newRow.
	secondRow
		addMorph: selectorListView proportionalWidth: 0.5;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.5.

	self layoutMorph
		addMorph: firstRow fixedHeight: self defaultButtonPaneHeight+4;
		addAdjusterAndMorph: secondRow proportionalHeight: 0.5;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.5.
	model changed: #editSelection! !

