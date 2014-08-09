'From Cuis 4.2 of 25 July 2013 [latest update: #2062] on 6 August 2014 at 8:51:12.4484 am'!

!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 8/3/2014 00:45'!
preDebugButtonSpec

	^{
	{'Proceed'.		#proceed. 	'continue execution'}.
	{'Abandon'.		#abandon. 	'abandon this execution by closing this window'}.
	{'Debug'.		#debug.		'bring up a debugger'}}! !

!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 8/2/2014 23:32'!
windowColor
	^ Theme current debugger! !


!TranscriptWindow methodsFor: 'GUI building' stamp: 'jmv 8/2/2014 22:12'!
windowColor
	^Theme current transcript! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 8/2/2014 22:06'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"

"MessageNames openMessageNames"

	| selectorListView firstRow searchButton secondRow |
	textMorph _ TextModelMorph
		textProvider: model
		textGetter: #searchString 
		textSetter: #searchString:
		selectionGetter: #contentsSelection
		allowStyler: false.
	textMorph textMorph setProperty: #alwaysAccept toValue: true.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph setTextColor: Color brown.
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


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 8/2/2014 22:13'!
buildMorphicWindowMessage: messageString
	| notifyPane |
	model expandStack.
	messageString
		ifNil: [
			notifyPane _ PluggableListMorph
				model: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: nil
				mainView: self
				menuGetter: nil
				keystrokeAction: nil ]
		ifNotNil: [
			notifyPane _ TextModelMorph withText: messageString.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8! !

!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 8/6/2014 08:49'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	(aDebugger interruptedContext selector == #doesNotUnderstand:) ifTrue: [
		triads add: { 'Create'. #createMethod. 'create the missing method' }
	].
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row color: Color white.
	row separation: 1.
	row addMorphs: buttons.
	^row! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 8/2/2014 23:47'!
open: model label: aString message: messageString
	|  window |
	window _ self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString.
	aString ifNotNil: [ window setLabel: aString ].
	 window openInWorld! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'jmv 8/2/2014 22:12'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win |
	win _ TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	win layoutMorph addMorph: TranscriptMorph new proportionalHeight: 1.
	win openInWorld! !

!methodRemoval: PreDebugWindow #preDebugButtonQuads!
PreDebugWindow removeSelector: #preDebugButtonQuads!
