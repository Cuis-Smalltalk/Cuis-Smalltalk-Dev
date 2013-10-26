'From Cuis 4.0 of 21 April 2012 [latest update: #1396] on 21 August 2012 at 8:38:41 pm'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:37'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ xtent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:37'!
morphExtentInOwner
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ xtent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:38'!
morphHeight

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ xtent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:37'!
morphWidth

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	^ xtent x! !


!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 20:36'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self morphBoundsInWorld insetBy: borderWidth! !

