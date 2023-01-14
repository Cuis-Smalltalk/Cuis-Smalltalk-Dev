'From Cuis 6.0 [latest update: #5630] on 13 January 2023 at 8:02:49 pm'!

!StringRequestMorph methodsFor: 'accessing' stamp: 'jmv 1/13/2023 20:00:37'!
response
	^ response copy
! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 16:06:31'!
addTextPane
	| result |
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	self addMorph: result layoutSpec: LayoutSpec useAll.! !


!FillInTheBlankMorph methodsFor: 'accessing' stamp: 'jmv 1/13/2023 20:00:43'!
response

	^ response copy! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'pb 5/5/2019 18:39:46'!
createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
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

