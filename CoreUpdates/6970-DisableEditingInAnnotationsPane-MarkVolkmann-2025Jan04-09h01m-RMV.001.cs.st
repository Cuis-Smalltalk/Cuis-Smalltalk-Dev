'From Cuis7.3 [latest update: #6962] on 4 January 2025 at 9:03:45 am'!

!CodeWindow methodsFor: 'GUI building' stamp: 'RMV 1/4/2025 09:01:58'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph := (TextModelMorph
		textProvider: model
		textGetter: #annotation) emptyTextDisplayMessage: 'Class or method annotation (not selected?)'.
	aTextMorph disableEditing.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	model when: #decorateButtons send: #decorateButtons to: self.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !

