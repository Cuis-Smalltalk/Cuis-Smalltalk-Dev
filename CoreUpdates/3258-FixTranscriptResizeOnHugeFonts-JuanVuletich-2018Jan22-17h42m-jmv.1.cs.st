'From Cuis 5.0 of 7 November 2016 [latest update: #3257] on 22 January 2018 at 5:54:08 pm'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 1/22/2018 17:53:13'!
displayOn: aForm in: aRectangle
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f canvas innerR |
	aForm fill: aRectangle fillColor: Color white.
	font _ AbstractFont default.

	innerR _ aRectangle insetBy: self padding.
	fh _ font height.
	count _ innerR height // fh.
	x _ innerR left.
	y _ innerR top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	[ i <= lastIndex ] whileTrue: [
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

