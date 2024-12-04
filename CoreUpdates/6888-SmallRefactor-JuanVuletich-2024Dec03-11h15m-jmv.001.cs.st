'From Cuis7.1 [latest update: #6886] on 3 December 2024 at 11:16:15 am'!

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 12/3/2024 11:09:54'!
labelXPosition
	| f |
	f := Preferences at: #windowTitleFont.
	^ f lineSpacing * 5 + borderWidth! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 12/3/2024 11:10:25'!
drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f := Preferences at: #windowTitleFont.
	x0 :=  self labelXPosition.
	y0 := borderWidth * 6 // 10.
	availableW := extent x - x0.
	l := labelString.
	w := f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l := l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w := f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

