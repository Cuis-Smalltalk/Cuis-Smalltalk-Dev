'From Cuis 4.2 of 25 July 2013 [latest update: #2680] on 16 February 2016 at 4:43:41.251338 pm'!

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 2/16/2016 16:42'!
scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs |
	grabPosition ifNotNil: [
		handPositionRelativeToUs _ slider externalize: handPositionRelativeToSlider.
		v _ (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - self buttonExtent * 1.0
					/ self freeSliderRoom.
		self setValue: v ]! !

