'From Cuis 6.0 [latest update: #5630] on 13 January 2023 at 3:35:10 pm'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 1/13/2023 15:29:39'!
addTextPane
	| result |
	result _ (TextModelMorph
		textProvider: self
		textGetter: nil
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	result model actualContents: response.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	self addMorph: result layoutSpec: LayoutSpec useAll.! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 1/13/2023 15:29:46'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result _ (TextModelMorph
		textProvider: self
		textGetter: nil
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	result model actualContents: response.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: acceptBoolean;
		escAction: [ self cancelClicked ];
		morphExtent: `18 @ 5` * self sizeUnit.
	self
		addMorph: result
		position: `1 @ 2` * self sizeUnit.
	^ result.! !

