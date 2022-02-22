'From Cuis 6.0 [latest update: #5074] on 22 February 2022 at 4:59:29 pm'!

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 2/22/2022 16:57:50'!
layerNumber
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	stayUp ifTrue: [ ^super layerNumber ].
	^20! !

