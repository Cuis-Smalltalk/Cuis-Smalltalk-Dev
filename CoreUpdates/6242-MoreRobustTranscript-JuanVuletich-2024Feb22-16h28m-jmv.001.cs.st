'From Cuis6.3 [latest update: #6241] on 22 February 2024 at 4:29:04 pm'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 2/22/2024 16:28:42'!
displayTextOn: aCanvasOrSelf in: aRectangle
	"See senders"
	| font count x y fh innerR index |
	font := FontFamily defaultFamilyAndPointSize.
	innerR := aRectangle insetBy: self padding.
	x := innerR left.
	y := innerR top.
	fh := font lineSpacing.
	count := lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index := lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		(entries at: index) ifNotNil: [ :string |
			aCanvasOrSelf drawString: string at: x@y font: font color: Theme current text.
			y := y + fh ].
		index := index \\ self maxEntries + 1 ].
	aCanvasOrSelf drawString: unfinishedEntry contents at: x@y font: font color: Theme current text! !

