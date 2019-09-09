'From Cuis 5.0 [latest update: #3846] on 9 September 2019 at 8:58:25 am'!

!Object methodsFor: 'associating' stamp: 'HAW 9/9/2019 08:57:04'!
-> anObject
	"Answer an Association between self and anObject"

	^Association key: self value: anObject! !


!Browser methodsFor: 'class list' stamp: 'HAW 9/9/2019 08:56:20'!
createHierarchyTreeOf: col

	"Create a tree from a flat collection of classes"

	| transformed |

	transformed := col collect: [:ea | 
		| childs indexes |
		childs := col select: [:class | class superclass = ea].
		indexes := childs collect: [:child | col indexOf: child].
		Association key: ea value: indexes].
	transformed copy do: [:ea |
		ea value: (ea value collect: [:idx | 
			| val |
			val := transformed at: idx.
			transformed at: idx put: nil.
			val])].
	^ transformed select: [:ea | ea notNil].
! !

