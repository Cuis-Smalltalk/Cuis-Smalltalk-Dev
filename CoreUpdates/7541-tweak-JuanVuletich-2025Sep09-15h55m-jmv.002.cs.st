'From Cuis7.5 [latest update: #7538] on 9 September 2025 at 4:08:57 pm'!

!TextEditor methodsFor: 'events' stamp: 'jmv 9/9/2025 16:08:29'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	| b |

	initialSelectionStart := nil.
	initialSelectionStop := nil.
	doWordSelection := false.
	doParagraphSelection := false.

	b := textComposition characterBlockAtPoint: localEventPosition.
	(textComposition clickAt: localEventPosition) ifTrue: [
		markBlock := b.
		pointBlock := b.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			markBlock := b.
			pointBlock := b.	
			self setEmphasisHereFromText ]! !

