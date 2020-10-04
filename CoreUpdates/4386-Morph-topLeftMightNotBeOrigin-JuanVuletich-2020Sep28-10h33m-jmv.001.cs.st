'From Cuis 5.0 [latest update: #4382] on 28 September 2020 at 11:22:59 am'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/28/2020 11:03:19'!
morphContainsPoint: aLocalPoint
	"Not very good. False positives for non-rectangular morphs.
	Only useful as a backstop if the Canvas can't do better."

	^ self morphLocalBounds containsPoint: aLocalPoint! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2020 11:19:35'!
morphTopLeft
	"Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"
	^`0@0`! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/28/2020 10:57:58'!
morphPositionInWorld

	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!!"

	^self externalizeToWorld: `0@0`! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/28/2020 11:20:07'!
morphTopLeft
	"By default, morphs occupy a rectangle specified by #morphExtent"
	^self morphExtent // 2 negated! !

!methodRemoval: Morph #morphBounds stamp: 'jmv 9/28/2020 10:45:52'!
Morph removeSelector: #morphBounds!
