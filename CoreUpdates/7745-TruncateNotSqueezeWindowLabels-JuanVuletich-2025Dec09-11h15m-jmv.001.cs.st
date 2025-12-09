'From Cuis7.5 [latest update: #7744] on 9 December 2025 at 11:20:04 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/Dec/2025 11:19:30'!
drawLabelOn: aCanvas

	| x0 y0 f l availableW w |
	f := Preferences at: #windowTitleFont.
	x0 :=  self labelXPosition.
	y0 := borderWidth * 6 // 10.
	l := labelString.
	availableW := extent x - x0.
	w := f widthOfString: l.
	[ w > availableW ] whileTrue: [
		"l := l squeezedTo: (1.0 * l size * availableW / w) truncated."
		l := l truncateTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w := f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

