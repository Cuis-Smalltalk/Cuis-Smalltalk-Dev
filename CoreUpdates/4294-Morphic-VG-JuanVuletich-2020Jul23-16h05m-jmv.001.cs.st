'From Cuis 5.0 [latest update: #4288] on 23 July 2020 at 4:08:26 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 7/23/2020 16:08:22'!
invalidateLocalRect: localRectangle

	self invalidateDisplayRect: (self externalizeDisplayBounds: localRectangle) from: nil! !

