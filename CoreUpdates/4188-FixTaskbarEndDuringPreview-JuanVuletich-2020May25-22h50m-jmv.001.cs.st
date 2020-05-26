'From Cuis 5.0 [latest update: #4187] on 25 May 2020 at 10:51:22 pm'!

!Morph methodsFor: 'previewing' stamp: 'jmv 5/25/2020 22:49:11'!
endPreview
	
	self previewing ifTrue: [
		self visible: self visibleBeforePreview.
		owner notNil ifTrue: [ owner addMorph: self inFrontOf: self morphBehindBeforePreview ].
		self previewing: false. ]! !

