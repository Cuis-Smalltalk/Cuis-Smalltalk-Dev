'From Cuis 4.1 of 12 December 2012 [latest update: #1667] on 9 April 2013 at 1:20:37 pm'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/9/2013 12:58'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f bw r canvas |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/9/2013 12:58'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ StrikeFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas
		newClipRect: innerRectangle;
		drawString: string at: x@y font: font color: Color veryDarkGray! !

