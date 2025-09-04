'From Cuis7.5 [latest update: #7478] on 2 September 2025 at 12:41:07 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/2/2025 12:38:32'!
fullIncludesPixel: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape.
	See comment at #includesPixel: for important notes on behavior."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsDo: [ :m |
			(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]]].
	^ false.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/2/2025 12:37:42'!
allMorphsFrontToBackDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (including the receiver).
	Do it front to back. Any morph partially covering another is drawn first."

	self allSubmorphsFrontToBackDo: aBlock.
	aBlock value: self.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/2/2025 12:37:40'!
allSubmorphsFrontToBackDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (not including the receiver).
	Do it front to back. Any morph partially covering another is drawn first."

	submorphs do: [ :m |
		m allSubmorphsFrontToBackDo: aBlock.
		aBlock value: m ].! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/2/2025 12:40:43'!
morphVisibleAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint."

	| doWeIncludePixel |
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m morphVisibleAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

