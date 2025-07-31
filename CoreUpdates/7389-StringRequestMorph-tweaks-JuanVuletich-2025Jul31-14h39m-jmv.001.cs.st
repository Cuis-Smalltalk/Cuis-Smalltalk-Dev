'From Cuis7.5 [latest update: #7388] on 31 July 2025 at 2:40:18 pm'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/31/2025 14:39:59'!
addTextPane
	"Add a text pane and two buttons."
	
	| controlsContainer |
	textMorph := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textMorph emptyTextDisplayMessage: msg ].
	textMorph
		hasUnacceptedEdits: true; "Allow accepting initial contents"
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LinearLayoutMorph newRow)
		addMorphUseAll: textMorph;
		addMorph: 
			((PluggableButtonMorph model: self action: #ok)
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			((PluggableButtonMorph model: self action: #cancel)
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !

