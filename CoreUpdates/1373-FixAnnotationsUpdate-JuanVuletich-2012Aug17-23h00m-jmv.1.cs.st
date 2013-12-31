'From Cuis 4.0 of 21 April 2012 [latest update: #1372] on 17 August 2012 at 11:02:40 pm'!

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 8/17/2012 22:58'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ TextModelMorph
		textProvider: model
		textGetter: #annotation
		textSetter: nil
		selectionGetter: nil
		allowStyler: false.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !

