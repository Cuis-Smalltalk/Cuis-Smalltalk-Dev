'From Cuis 5.0 [latest update: #4744] on 10 August 2021 at 10:14:28 am'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 8/10/2021 10:13:48'!
refreshTaskbarFor: aMorph

	aMorph isIncludedInTaskbar
		ifTrue: [
			(self buttonFor: aMorph) ifNil: [self addButtonFor: aMorph ]]
		ifFalse: [ self removeButtonFor: aMorph ].! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 8/10/2021 10:10:57'!
stayUp: aBoolean

	stayUp _ aBoolean.
	aBoolean ifTrue: [ self removeStayUpBox ].
	self taskbar ifNotNil: [ :tb | tb refreshTaskbarFor: self ].! !

!MenuMorph methodsFor: 'testing' stamp: 'jmv 8/10/2021 10:00:57'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ stayUp! !

