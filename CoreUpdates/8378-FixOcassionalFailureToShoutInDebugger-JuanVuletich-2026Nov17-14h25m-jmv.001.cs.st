'From Cuis7.9 [latest update: #8377] on 17 November 2026 at 2:26:05 pm'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 17/Nov/2026, 5:24:17 pm (UTC)'!
actualContents: aTextOrString
	| actuallyChanged |
	actuallyChanged := self basicActualContents: aTextOrString.
	actuallyChanged | self stylesText
		ifTrue: [
			self changed: #actualContents ].
	^actuallyChanged! !

