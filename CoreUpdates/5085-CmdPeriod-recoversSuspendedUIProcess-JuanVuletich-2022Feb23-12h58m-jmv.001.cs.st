'From Cuis 6.0 [latest update: #5084] on 23 February 2022 at 1:02:23 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 2/23/2022 12:58:56'!
newUIProcessIfNeeded
	"Answer is not nil only if a new process was created."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isSuspended ifTrue: [ oldUIProcess terminate ].
		oldUIProcess isTerminated ifTrue: [
			oldUIProcess 	animatedUI ifNotNil: [ :guiRootObject |
				self spawnNewMorphicProcessFor: guiRootObject.
				^UIProcess ]]].
	^nil! !

