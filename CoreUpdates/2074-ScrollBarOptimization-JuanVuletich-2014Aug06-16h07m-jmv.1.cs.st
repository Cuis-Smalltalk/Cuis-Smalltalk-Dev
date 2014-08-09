'From Cuis 4.2 of 25 July 2013 [latest update: #2073] on 6 August 2014 at 4:16:35.58875 pm'!

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 8/6/2014 16:15'!
privateExtent: aPoint
	| isH wasH |
	wasH _ self isHorizontal.
	^ (super privateExtent: aPoint)
		ifTrue: [
			"Doesn't move!!"
			"upButton morphPosition: borderWidth@borderWidth."
			downButton morphPosition: extent - borderWidth - downButton morphExtent.
			isH _ self isHorizontal.
			isH = wasH ifFalse: [
				isH
					ifTrue: [
						upButton updateLeftButtonImage.
						downButton updateRightButtonImage ]
					ifFalse: [
						upButton updateUpButtonImage.
						downButton updateDownButtonImage ]].
			]; yourself! !

