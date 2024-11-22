'From Cuis7.1 [latest update: #6836] on 22 November 2024 at 9:30:06 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/22/2024 09:27:49'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self evaluateSelectionAndDo: nil ifFail: nil profiled: aBoolean.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/22/2024 09:28:33'!
exploreIt

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ ^morph flash ]
		profiled: false.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/22/2024 09:29:03'!
inspectIt

	| hadSelection |
	hadSelection := self hasSelection.
	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ ^morph flash ]
		profiled: false.
	hadSelection ifFalse: [
		self deselectAndPlaceCursorAt: self stopIndex ].
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

