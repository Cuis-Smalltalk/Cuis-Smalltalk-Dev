'From Cuis 5.0 [latest update: #4063] on 9 March 2020 at 10:14:50 am'!

!Morph methodsFor: 'testing' stamp: 'jmv 3/9/2020 10:11:38'!
isOwnedByWorld
	^owner is: #PasteUpMorph! !


!PasteUpMorph methodsFor: 'testing' stamp: 'jmv 3/9/2020 10:13:47'!
is: aSymbol
	^ aSymbol == #PasteUpMorph or: [ super is: aSymbol ]! !


!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 2/24/2020 14:16:35'!
addMorphFrontFromWorldPosition: aMorph

	super addMorphFrontFromWorldPosition: aMorph.
	self refreshExtent.
! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 3/9/2020 09:55:00'!
fontPreferenceChanged
	self recreateDefaultDesktop.
	self submorphsDo: [ :m |
		m morphExtent: (m morphExtent max: m minimumExtent).
		m fontPreferenceChanged ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 3/9/2020 09:52:24'!
minimumExtent

	^layoutMorph minimumExtent + (borderWidth * 2) + (0@self labelHeight) max: self titleBarButtonsExtent x * 6 @ 0! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 3/9/2020 10:14:14'!
refreshExtent
	"Flush cache & recalculate"
	(self isOwnedByWorld or: [self isOwnedByHand]) ifTrue: [
		self morphExtent: (self morphExtent max: self minimumExtent) ]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 2/22/2020 11:41:51'!
beColumn
	direction _ #vertical.
	self padding: #center.
	self refreshExtent.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 2/22/2020 11:41:57'!
beRow
	direction _ #horizontal.
	self padding: #left.
	self refreshExtent.! !

