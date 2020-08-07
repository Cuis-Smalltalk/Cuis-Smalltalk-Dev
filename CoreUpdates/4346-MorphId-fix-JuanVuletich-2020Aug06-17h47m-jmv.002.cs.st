'From Cuis 5.0 [latest update: #4344] on 6 August 2020 at 7:39:03 pm'!
"Make all ids notNil"
Morph allSubInstances do: [ :m | (m instVarNamed: 'id') ifNil: [ m instVarNamed: 'id' put: 0 ]]!

!Morph methodsFor: 'accessing' stamp: 'jmv 8/6/2020 19:37:15'!
morphId
	"Non zero. Zero id means no Morph.
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

!Morph methodsFor: 'caching' stamp: 'jmv 8/6/2020 19:36:23'!
clearId
	"But keep flags.
	Morph clearIds
	"

	id _ id bitAnd: 255.! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/6/2020 19:31:28'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.
	id _ 0.! !

!Morph methodsFor: 'private' stamp: 'jmv 8/6/2020 19:37:39'!
privateFlagAt: bitIndex
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	^(id bitAt: bitIndex) = 1! !

!Morph methodsFor: 'private' stamp: 'jmv 8/6/2020 19:37:46'!
privateFlagAt: bitIndex put: aBoolean
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	id _ id bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/6/2020 18:05:52'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb |
	savedPatch _ prevSavedPatch.
	((savedPatch is: #Form) not or: [savedPatch extent ~= aRectangle extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: aRectangle extent depth: form depth ]
		ifFalse: [
			savedPatch offset: 0@0 ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	savedPatch offset: aRectangle topLeft.
	^savedPatch! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Morph clearIds!

