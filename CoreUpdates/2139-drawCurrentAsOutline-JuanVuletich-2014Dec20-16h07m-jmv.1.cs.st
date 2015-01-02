'From Cuis 4.2 of 25 July 2013 [latest update: #2138] on 20 December 2014 at 4:18:17.094871 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/20/2014 16:13'!
drawCurrentAsOutline

	(self isMorphVisible: currentMorph) ifTrue: [
		self
			reverseRectangleBorder: currentMorph morphLocalBounds
			borderWidth: 2 ]! !

