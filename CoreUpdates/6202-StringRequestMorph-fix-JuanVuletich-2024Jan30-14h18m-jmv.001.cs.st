'From Cuis6.3 [latest update: #6201] on 30 January 2024 at 2:19:51 pm'!
!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 1/30/2024 14:18:40'!
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
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LayoutMorph newRow)
		addMorphUseAll: textMorph;
		addMorph: 
			(PluggableButtonMorph model: textMorph scroller action: #acceptContents ::
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			(PluggableButtonMorph model: self action: #cancel ::
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 1/30/2024 14:18:27'!
textBox
	
	^textMorph! !

!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock textMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
