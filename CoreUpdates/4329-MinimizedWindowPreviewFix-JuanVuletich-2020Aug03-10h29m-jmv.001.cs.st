'From Cuis 5.0 [latest update: #4327] on 3 August 2020 at 10:31:05 am'!

!Morph methodsFor: 'previewing' stamp: 'jmv 8/3/2020 10:30:35'!
endPreview
	
	self previewing ifTrue: [
		owner notNil ifTrue: [
			self redrawNeeded.
			self visible: self visibleBeforePreview.
			owner addMorph: self inFrontOf: self morphBehindBeforePreview ].
		self previewing: false. ]! !

