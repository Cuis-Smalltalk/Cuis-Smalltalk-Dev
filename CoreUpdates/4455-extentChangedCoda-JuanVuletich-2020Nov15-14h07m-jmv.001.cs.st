'From Cuis 5.0 [latest update: #4453] on 15 November 2020 at 2:26:04 pm'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/15/2020 14:25:21'!
textSize
	actualContents ifNil: [ ^0 ].
	^actualContents size! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 11/15/2020 14:25:48'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent oldExtent |
	wrapFlag _ true.
	model _ aTextModel.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		extent _ newExtent.
		self extentChanged: oldExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self model: aTextModel! !

