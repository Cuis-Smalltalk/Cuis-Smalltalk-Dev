'From Cuis 4.2 of 25 July 2013 [latest update: #2262] on 20 April 2015 at 5:05:12.053539 pm'!
!classDefinition: #RealEstateAgent category: #'System-Support'!
Object subclass: #RealEstateAgent
	instanceVariableNames: ''
	classVariableNames: 'ReverseStaggerOffset StaggerOffset StaggerOrigin StandardSize StandardWindowOrigins '
	poolDictionaries: ''
	category: 'System-Support'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 4/20/2015 17:04'!
staggerOffset
	^6 @ 20! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 4/20/2015 17:04'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	Preferences reverseWindowStagger 
		ifTrue: 
			[^self 
				strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld].
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w morphBoundsInWorld].
	allOrigins := otherFrames collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld) do: [ :aPosition | 
			"First see if one of the standard positions is free"
			(allOrigins includes: aPosition) 
				ifFalse: [
					^(aPosition extent: initialExtent) translatedAndSquishedToBeWithin: allowedArea]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	
	[putativeOrigin := putativeOrigin + self staggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom 
		and: [putativeFrame right < screenRight]] 
			whileTrue: [
				(allOrigins includes: putativeOrigin) 
					ifFalse: [
						^(putativeOrigin extent: initialExtent) 
							translatedAndSquishedToBeWithin: allowedArea]].
	^(self scrollBarSetback @ self screenTopSetback extent: initialExtent) 
		translatedAndSquishedToBeWithin: allowedArea! !

!methodRemoval: RealEstateAgent class #initialize!
RealEstateAgent class removeSelector: #initialize!
RealEstateAgent initialize!
!classDefinition: #RealEstateAgent category: #'System-Support'!
Object subclass: #RealEstateAgent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
