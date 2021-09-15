'From Cuis 5.0 [latest update: #4433] on 26 October 2020 at 1:18:49 pm'!

!WidgetMorph methodsFor: 'geometry' stamp: 'KenD 10/26/2020 13:16:01'!
ensureMinimimExtent

	self privateExtent: extent! !


!WidgetMorph methodsFor: 'accessing' stamp: 'KenD 10/26/2020 13:16:39'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth _ anInteger max: 0.
		self  ensureMinimimExtent;
			redrawNeeded ]! !


!LabelMorph methodsFor: 'geometry' stamp: 'KenD 10/26/2020 13:02:40'!
minimumExtent

	^ self measureContents + self extentBorder! !

