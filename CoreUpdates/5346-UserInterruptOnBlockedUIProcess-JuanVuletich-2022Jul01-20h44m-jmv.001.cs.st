'From Cuis 6.0 [latest update: #5344] on 1 July 2022 at 8:47:11 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/1/2022 20:47:04'!
newUIProcessIfNeeded
	"Answer is not nil only if a new process was created."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			oldUIProcess 	animatedUI ifNotNil: [ :guiRootObject |
				self spawnNewMorphicProcessFor: guiRootObject.
				^oldUIProcess ]]].
	^nil! !

