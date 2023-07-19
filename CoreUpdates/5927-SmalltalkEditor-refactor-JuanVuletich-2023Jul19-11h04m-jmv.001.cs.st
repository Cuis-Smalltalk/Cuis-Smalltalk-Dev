'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 11:30:29 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 11:09:02'!
inspectIt

	self hasSelection ifFalse: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass |
				self selectNodeUnderCursorForInspectionIn: methodNode ]
			ifErrorsParsing: [ :anError | ]].

	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!methodRemoval: SmalltalkEditor #inspectSelectionOrLine stamp: 'jmv 7/19/2023 11:09:05'!
SmalltalkEditor removeSelector: #inspectSelectionOrLine!
