'From Cuis 5.0 [latest update: #4191] on 27 May 2020 at 12:05:23 am'!

!Morph methodsFor: 'previewing' stamp: 'jmv 5/27/2020 00:04:36'!
endPreview
	
	self previewing ifTrue: [
		owner notNil ifTrue: [
			self visible: self visibleBeforePreview.
			owner addMorph: self inFrontOf: self morphBehindBeforePreview ].
		self previewing: false. ]! !

