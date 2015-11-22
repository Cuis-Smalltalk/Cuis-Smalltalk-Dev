'From Cuis 4.2 of 25 July 2013 [latest update: #2592] on 18 November 2015 at 10:39:02.352509 pm'!

!TextComposition methodsFor: 'display' stamp: 'len 11/18/2015 22:35'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d newLastTextCursorRect |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
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
	x0-textLeft < -1 ifTrue: [
		x1 _ x1 - x0 + textLeft.
		x0 _ textLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-textLeft) ifTrue: [
		x0 _ x0 + r - x1+textLeft.
		x1 _ r +textLeft].
	newLastTextCursorRect _ isBold
		ifTrue: [ x0-halfW@ top corner: x1+halfW+3 @ (bottom+1) ]
		ifFalse: [ x0-halfW@ top corner: x1+halfW+2 @ (bottom+1) ].
	lastTextCursorRect _ lastTextCursorRect isNil
		ifTrue: [newLastTextCursorRect]
		ifFalse: [lastTextCursorRect merge: newLastTextCursorRect].
	aCanvas
		line: x0+halfW@(bottom-w-w+2+halfW) to: x1+halfW@top
		width: w color: textCursorColor! !

