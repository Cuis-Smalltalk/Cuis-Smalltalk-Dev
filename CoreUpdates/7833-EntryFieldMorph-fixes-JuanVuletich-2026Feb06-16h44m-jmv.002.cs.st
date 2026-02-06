'From Cuis7.7 [latest update: #7830] on 6 February 2026 at 5:15:14 pm'!

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 6/Feb/2026 17:13:54'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: anEmphasis on: aCanvas
	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold := anEmphasis allMask: 1.
	isItalic := anEmphasis allMask: 2.
	textCursorColor := Theme current textCursor.
	h := bottom - top.
	w := isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW := w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d := isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 := x- (h*5//24) + d.
			x1 := x + d ]
		ifFalse: [
			x0 := x.
			x1 := x].
	x0 < halfW ifTrue: [
		x1 := x1 - x0 + halfW.
		x0 := halfW ].
	r := extent x-halfW-1.
	r < x1 ifTrue: [
		x0 := x0 + r - x1.
		x1 := r ].
	textCursorRect := x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0-halfW@(bottom-w) to: x1-halfW@top
		width: w color: textCursorColor.! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 6/Feb/2026 16:53:37'!
drawOn: aCanvas

	| delta f |
	aCanvas
		fillRectangle: self localBounds
		color: backgroundColor
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.
		
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	
	self hasTextCursor ifTrue: [ self drawTextCursorOn: aCanvas ].
	
	f := self fontToUse.
	delta := contents isEmpty
		ifFalse: [ (f boundsLeftOf: contents first) negated@0 ]
		ifTrue: [ 0@0 ].
	aCanvas
		drawString: contents
		at: self textOffset + delta
		font: f
		color: color.
! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 6/Feb/2026 17:01:41'!
drawSelectionOn: aCanvas
	| rightX leftX bottom f |
	f := self fontToUse.
	bottom := self measureContents y + self textOffset y.
	leftX := f widthOfString: contents from: 1 to: editor startIndex-1.
	editor startIndex > 1 ifTrue: [
		leftX := leftX + (f boundsLeftOf: (contents at: editor startIndex)) ].
	leftX := (leftX + self textOffset x) min: extent x.
	rightX := f widthOfString: contents from: 1 to: editor stopIndex-1.
	editor stopIndex > contents size ifFalse: [
		rightX := rightX + (f boundsLeftOf: (contents at: editor stopIndex)) ].
	rightX := (rightX + self textOffset x) min: extent x.

	aCanvas
		fillRectangle: (leftX @ self textOffset y corner: rightX @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!EntryFieldMorph methodsFor: 'drawing' stamp: 'jmv 6/Feb/2026 17:14:40'!
drawTextCursorOn: aCanvas
	|  x f |

	showTextCursor ifTrue: [
		f := self fontToUse.
		x := f widthOfString: contents from: 1 to: editor startIndex-1.
		(editor startIndex > 1 and: [contents size >= editor startIndex]) ifTrue: [
			x := x + (f boundsLeftOf: (contents at: editor startIndex)) ].
		self displayTextCursorAtX: (x + self textOffset x)  
			top: self textOffset y
			bottom: self measureContents y + self textOffset y 
			emphasis: emphasis 
			on: aCanvas ]! !

!EntryFieldMorph methodsFor: 'geometry' stamp: 'jmv 6/Feb/2026 16:52:16'!
textOffset
	"Answer offset to upper-left of text drawing origin"
	^ (2 * self borderWidth) @ ( (2 * self borderWidth) + self fontToUse descent)! !

