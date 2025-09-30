'From Cuis7.5 [latest update: #7625] on 30 September 2025 at 3:12:31 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/30/2025 15:12:02'!
addMorphBack: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	
	^self privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])! !

