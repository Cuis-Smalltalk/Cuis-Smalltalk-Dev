'From Cuis 4.2 of 25 July 2013 [latest update: #1798] on 13 August 2013 at 9:07:52.700481 pm'!

!Paragraph methodsFor: 'display' stamp: 'jmv 8/13/2013 21:06'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas paragraphLeft: paragraphLeft
	"x, top, bottom, paragraphLeft are relative to the morph currently being drawn."

	| caretColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	caretColor _ Theme current insertionPoint.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x - halfW.
			x1 _ x - halfW].
	x0-paragraphLeft < -1 ifTrue: [
		x1 _ x1 - x0 + paragraphLeft.
		x0 _ paragraphLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-paragraphLeft) ifTrue: [
		x0 _ x0 + r - x1+paragraphLeft.
		x1 _ r +paragraphLeft].
	lastCaretRect _ isBold
		ifTrue: [ x0-halfW@ top corner: x1+halfW+3 @ (bottom+2) ]
		ifFalse: [ x0-halfW@ top corner: x1+halfW+2 @ (bottom+2) ].
	aCanvas
		line: x0@(bottom-w-w+3) to: x1@(top-halfW)
		width: w color: caretColor! !

