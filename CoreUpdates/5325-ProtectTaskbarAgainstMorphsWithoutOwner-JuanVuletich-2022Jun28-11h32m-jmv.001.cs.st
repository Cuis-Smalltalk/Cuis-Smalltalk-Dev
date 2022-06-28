'From Cuis 6.0 [latest update: #5324] on 28 June 2022 at 11:35:01 am'!

!Morph methodsFor: 'user interface' stamp: 'jmv 6/28/2022 11:33:14'!
toggleCollapseOrShow
	"If collapsed, show me.
	If visible, collapse me."

	owner ifNil: [ ^self ].
	(self visible and: [self atFront])
		ifTrue:  [ self collapse ]
		ifFalse: [ self showAndComeToFront ]! !

!Morph methodsFor: 'previewing' stamp: 'jmv 6/28/2022 11:32:56'!
beginPreview

	owner ifNil: [ ^self ].
	(self visible and: [self atFront]) ifFalse: [
		self visibleBeforePreview: self visible.
		self morphBehindBeforePreview: (self owner submorphBehind: self).
		self previewing: true.
		self showAndComeToFront. ]! !

