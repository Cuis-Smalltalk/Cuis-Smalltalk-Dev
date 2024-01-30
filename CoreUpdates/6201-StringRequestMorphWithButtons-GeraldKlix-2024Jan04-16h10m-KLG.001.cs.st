'From Haver6.3 [latest update: #6163] on 4 January 2024 at 4:10:13 pm'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'KLG 1/4/2024 16:06:50'!
addTextPane
	"Add a text pane and two buttons."
	
	| textPane controlsContainer |
	textPane := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textPane emptyTextDisplayMessage: msg ].
	textPane
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LayoutMorph newRow)
		addMorphUseAll: textPane;
		addMorph: 
			(PluggableButtonMorph model: textPane scroller action: #acceptContents ::
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			(PluggableButtonMorph model: self action: #cancel ::
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !

