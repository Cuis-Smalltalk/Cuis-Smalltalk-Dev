'From Cuis 4.2 of 25 July 2013 [latest update: #2332] on 22 May 2015 at 1:39:38.919582 pm'!

!BitBlt methodsFor: 'private' stamp: 'jmv 5/22/2015 13:39'!
setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin
	"Set up a BitBlt for copying a Rectangle as large as possible, with sourceOrigin and destOrigin. Bound by sourceForm and destForm extents. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf _ FloatImage lena asForm.
	sourceOrigin _ 30@30.
	df _ Form extent: 156@156 depth: 32.
	df fillColor: Color red.
	destOrigin _ 10@20.
	blitter _ BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin.
	blitter copyBits.
	df display
	"
	self
		setDestForm: df sourceForm: sf
		destOrigin: destOrigin sourceOrigin: sourceOrigin
		extent: (sf width - sourceOrigin x) @ (sf height - sourceOrigin y)! !

!BitBlt methodsFor: 'private' stamp: 'jmv 5/22/2015 12:00'!
setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent
	"Set up a BitBlt for copying a Rectangle of extent w@h , with sourceOrigin and destOrigin. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf _ FloatImage lena asForm.
	sourceOrigin _ 30@30.
	sourceOrigin _ 0@0.
	df _ Form extent: 256@256 depth: 32.
	df fillColor: Color red.
	destOrigin _ 0@0.
	blitter _ BitBlt new
		setSourceForm: sf sourceOrigin: sourceOrigin
		destForm: df destOrigin: destOrigin
		width: 256 height: 256.
	blitter copyBits.
	df display
	"
	self setDestForm: df.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceForm _ sf.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ extent x.
	height _ extent y.
	halftoneForm _ nil.
	combinationRule _ 3. 	"Form over"
	colorMap _ nil! !


!BitBlt methodsFor: 'private' stamp: 'jmv 5/22/2015 11:55'!
setDestForm: df

	destForm _ df.
	clipX _ 0.
	clipY _ 0.
	clipWidth _ df width.
	clipHeight _ df height! !

!methodRemoval: BitBlt #setSourceForm:sourceOrigin:destForm:destOrigin:!
BitBlt removeSelector: #setSourceForm:sourceOrigin:destForm:destOrigin:!
!methodRemoval: BitBlt #setSourceForm:sourceOrigin:destForm:destOrigin:width:height:!
BitBlt removeSelector: #setSourceForm:sourceOrigin:destForm:destOrigin:width:height:!
