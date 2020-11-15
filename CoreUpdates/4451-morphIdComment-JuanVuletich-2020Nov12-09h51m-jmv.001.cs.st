'From Cuis 5.0 [latest update: #4450] on 12 November 2020 at 10:50:18 am'!

!Morph methodsFor: 'accessing' stamp: 'jmv 11/12/2020 09:51:47'!
morphId
	"Non zero. A zero id in, for example, VectorEngine, means no Morph.
	reserve lower 8 bits of numeric ivar for boolean flags."
	"
	Morph clearIds
	"
	| morphId |
	morphId _ id >> 8.
	morphId = 0 ifTrue: [
		LastMorphId isNil ifTrue: [ LastMorphId _ 0 ].
		LastMorphId _ LastMorphId + 1.
		id _ LastMorphId << 8 + id.							"Keep any flags"
		morphId _ LastMorphId ].
	^morphId! !

