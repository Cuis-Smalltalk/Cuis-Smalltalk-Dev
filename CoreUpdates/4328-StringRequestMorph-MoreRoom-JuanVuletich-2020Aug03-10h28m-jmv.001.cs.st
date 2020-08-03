'From Cuis 5.0 [latest update: #4327] on 3 August 2020 at 10:29:34 am'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/31/2020 21:32:04'!
addTextPane
	| result lineCount |
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
	"If the initial response is big or has multiple lines we make the text pane bigger and with scrollbars:"
	lineCount _ response lineCount.
	lineCount > 1
		ifTrue: [result morphExtent: 40 @ (lineCount*2) * FontFamily defaultLineSpacing]
		ifFalse:
			[result hideScrollBarsIndefinitely;
				morphExtent: response size @ 1 * FontFamily defaultLineSpacing + (0@8).].
	self addMorphBack: result.
	^ result.! !

