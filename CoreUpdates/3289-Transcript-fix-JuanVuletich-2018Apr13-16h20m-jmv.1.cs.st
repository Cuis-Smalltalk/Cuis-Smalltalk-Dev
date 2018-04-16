'From Cuis 5.0 of 7 November 2016 [latest update: #3283] on 13 April 2018 at 4:53:18 pm'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/13/2018 16:51:04'!
displayOn: aForm in: aRectangle
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOn: Display in: bounds
	"
	| font count string x y fh canvas innerR index |
	aForm fill: aRectangle fillColor: `Color white`.
	font _ AbstractFont default.

	innerR _ aRectangle insetBy: self padding.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	x _ innerR left.
	y _ innerR top.
	fh _ font height.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		canvas drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: `Color veryDarkGray`! !

