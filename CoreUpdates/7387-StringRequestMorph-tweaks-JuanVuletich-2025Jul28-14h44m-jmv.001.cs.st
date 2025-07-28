'From Cuis7.5 [latest update: #7386] on 28 July 2025 at 2:45:18 pm'!

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 7/28/2025 14:31:28'!
ok
	textMorph scroller acceptContents! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/28/2025 14:31:41'!
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
		hasUnacceptedEdits: false;
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

