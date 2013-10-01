'From Cuis 4.2 of 25 July 2013 [latest update: #1838] on 30 September 2013 at 8:50:58.494883 pm'!

!FormCanvas methodsFor: 'drawing' stamp: 'KenD 9/30/2013 16:08'!
line: pt1 to: pt2 width: w color: c
	|  p1 p2 |
	p1 _ currentTransformation intTransform: pt1.
	p2 _ currentTransformation intTransform: pt2.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: p1 to: p2! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/30/2013 20:40'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| br w w2 |
	br _ self morphExtent.
	w _ br > (20@20)
		ifTrue: [ 10 ]
		ifFalse: [ 1 ].
	aCanvas
		frameAndFillRectangle: (0@0 corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	w2 _ w//2.
	aCanvas line: 0@0 to: (br-w) width: w color: Color yellow.
	aCanvas line: (br x-w) @ 0 to: 0 @ (br y - w) width: w color: Color yellow.! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/30/2013 20:42'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
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
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ extent x-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	caretRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0+halfW@bottom to: x1+halfW@(top+w)
		width: w color: caretColor! !


!Paragraph methodsFor: 'display' stamp: 'jmv 9/30/2013 20:42'!
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
		line: x0+halfW@(bottom-w-w+3+halfW) to: x1+halfW@top
		width: w color: caretColor! !

