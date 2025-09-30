'From Cuis7.5 [latest update: #7632] on 30 September 2025 at 5:20:33 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 17:16:21'!
addMorph: newMorph behind: aMorph
	"Add a morph to the list of submorphs behind the specified morph"
	| aMorphIndex index |
	aMorphIndex := submorphs indexOf: aMorph.
	index := (newMorph owner == self and: [
		(submorphs indexOf: newMorph) < aMorphIndex])
			ifTrue: [aMorphIndex]
			ifFalse: [aMorphIndex + 1].
	^self privateAddMorph: newMorph atIndex: index! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 17:09:47'!
addMorph: newMorph inFrontOf: aMorph
	"Add a morph to the list of submorphs in front of the specified morph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph).! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 17:17:33'!
addMorphBack: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 17:06:29'!
addMorphBack: aMorph position: aPoint
	^self
		privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])
		position: aPoint! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 4/11/2013 08:48'!
sendToBack
	| thisWorld |
	thisWorld := self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorphBack: self ]! !

!methodRemoval: Morph #moveMorphBack: stamp: 'jmv 9/30/2025 17:06:55'!
Morph removeSelector: #moveMorphBack:!
