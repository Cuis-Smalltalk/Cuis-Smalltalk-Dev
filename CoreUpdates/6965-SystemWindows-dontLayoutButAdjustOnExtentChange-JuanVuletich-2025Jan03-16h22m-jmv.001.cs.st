'From Cuis7.3 [latest update: #6963] on 3 January 2025 at 4:40:41 pm'!

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 1/3/2025 16:39:08'!
extentChanged: oldExtent
	"Our extent changed.
	- Reposition adjusters
	- Resize our layoutMorph"

	| h thickness w cornerExtent wh ww pos halfThickness |
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
		pos := borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			position: pos
			extent: extent - pos - borderWidth ].! !

!methodRemoval: SystemWindow #layoutSubmorphs stamp: 'jmv 1/3/2025 16:39:18'!
SystemWindow removeSelector: #layoutSubmorphs!
