'From Cuis7.5 [latest update: #7664] on 21 October 2025 at 5:11:25 pm'!

!Text methodsFor: 'copying' stamp: 'jmv 10/21/2025 16:51:16'!
appendToText: aText

	| start textResult |
	start := aText size + 1.
	textResult := Text fromString: aText string , self string.
	textResult privateSetRuns: (aText runs copyReplaceFrom: start to: textResult size with: self runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	textResult fixParagraphAttributesFrom: start to: textResult size.
	^ textResult
	! !

