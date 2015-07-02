'From Cuis 4.2 of 25 July 2013 [latest update: #2392] on 30 June 2015 at 9:45:35.300103 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 6/30/2015 09:11'!
drawLabelOn: aCanvas

	| e x0 y0 f w availableW l |
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2.
	f _ Preferences windowTitleFont.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !

