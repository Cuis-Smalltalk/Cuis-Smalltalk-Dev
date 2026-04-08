'From Cuis7.7 [latest update: #7868] on 8 April 2026 at 3:34:11 pm'!

!Layout methodsFor: 'layout' stamp: 'jmv 8/Apr/2026 15:28:26'!
layoutMorphs: morphs in: boundsForLayout
	self subclassResponsibility! !


!LinearLayout methodsFor: 'adjust' stamp: 'jmv 8/Apr/2026 15:27:28'!
adjustMorphBefore: t morphAfter: b adjusterRefPos: adjusterRefPos at: localPoint
	self subclassResponsibility! !


!Layout reorganize!
('layout' layoutMorphs:in:)
('testing' isColumn isFormLayout isLinearLayout isRow)
!

