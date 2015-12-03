'From Cuis 4.2 of 25 July 2013 [latest update: #2602] on 29 November 2015 at 1:03:07.786299 pm'!

!TextComposition methodsFor: 'access' stamp: 'jmv 11/29/2015 10:29'!
getAndCleanLastTextCursorRect
	"Warning: Could be not updated, it is the one for when the text cursor was last drawn."
	| answer |
	answer _ lastTextCursorRect.
	lastTextCursorRect _ nil.
	^answer! !


!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 11/29/2015 10:29'!
onBlinkCursor
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textComposition getAndCleanLastTextCursorRect ifNotNil: [ :r | self invalidateLocalRect: r].! !


!TextComposition methodsFor: 'display' stamp: 'jmv 11/29/2015 10:28'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d extraW corner origin |
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
	extraW _ isBold
		ifTrue: [ 3 ]
		ifFalse: [ 2 ].
	origin _ x0-halfW@ top.
	corner _ x1+halfW+extraW @ (bottom+1).
	lastTextCursorRect
		ifNil: [ lastTextCursorRect _ origin corner: corner ]
		ifNotNil: [lastTextCursorRect
			setOrigin: (lastTextCursorRect origin min: origin)
			corner: (lastTextCursorRect corner max: corner)].
	aCanvas
		line: x0+halfW@(bottom-w-w+2+halfW) to: x1+halfW@top
		width: w color: textCursorColor! !

!methodRemoval: TextComposition #lastTextCursorRect!
TextComposition removeSelector: #lastTextCursorRect!
