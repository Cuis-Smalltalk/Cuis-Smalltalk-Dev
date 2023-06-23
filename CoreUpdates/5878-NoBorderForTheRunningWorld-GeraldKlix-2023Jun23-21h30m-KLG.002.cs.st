'From Haver 6.0 [latest update: #5876] on 23 June 2023 at 10:10:28 pm'!

!WorldMorph class methodsFor: 'instance creation' stamp: 'KLG 6/23/2023 22:08:00'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w := self new.
	w borderWidth: 0.
	w morphPosition: `0@0` extent: Display extent.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

self runningWorld ifNotNil: [ :w |
	w borderWidth: 0 ].!

