'From Cuis7.3 [latest update: #7028] on 28 January 2025 at 2:24:23 pm'!

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 1/27/2025 15:21:40'!
interiorExtent
	| p |
	p := super interiorExtent.
	^p x @ (p y - self labelHeight)! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 1/27/2025 15:21:19'!
interiorOrigin
	| p |
	p := super interiorOrigin.
	^p x @ (p y + self labelHeight)! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 1/27/2025 15:22:42'!
extentChanged: oldExtent
	"Our extent changed.
	- Reposition adjusters
	- Resize our layoutMorph"

	| h thickness w cornerExtent wh ww halfThickness |
	thickness := self borderWidth.
	halfThickness := thickness * 0.5.
	cornerExtent := thickness * 5.
	ww := extent x.
	wh := extent y.
	w := ww - cornerExtent - cornerExtent.
	h := wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) position: cornerExtent@halfThickness extent: w@0.
	(adjusters at: #bottomAdjuster) position: cornerExtent@(wh-halfThickness) extent: w@0.
	(adjusters at: #leftAdjuster) position: halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #rightAdjuster) position: ww-halfThickness@cornerExtent extent: 0@h.
	(adjusters at: #topLeftAdjuster) position: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) position: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) position: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) position: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].! !

