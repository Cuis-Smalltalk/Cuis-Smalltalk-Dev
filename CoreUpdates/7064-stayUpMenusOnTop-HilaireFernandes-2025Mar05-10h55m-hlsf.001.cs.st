'From Cuis7.3 [latest update: #7062] on 5 March 2025 at 10:56:02 am'!

!MenuMorph methodsFor: 'accessing' stamp: 'hlsf 3/2/2025 10:34:10'!
stayUp: aBoolean
	stayUp := aBoolean.
	aBoolean 
		ifTrue: [ self removeStayUpBox.
			self setProperty: #layerNumber toValue: 20]
		ifFalse: [self removeProperty: #layerNumber].
	self taskbar ifNotNil: [ :tb | tb refreshTaskbarFor: self ].! !

