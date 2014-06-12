'From Cuis 4.2 of 25 July 2013 [latest update: #2020] on 12 June 2014 at 9:06:47.768528 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/12/2014 09:04'!
addMorph: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	self addMorphFront: aMorph! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/12/2014 09:04'!
addMorphBack: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph atIndex: submorphs size+1! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/12/2014 09:04'!
addMorphFront: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph atIndex: 1! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 6/12/2014 09:06'!
addMorphFrontFromWorldPosition: aMorph

	| positionInWorld |
	self flag: #jmv. 	"Not really pretty..."
	positionInWorld _ aMorph morphPositionInWorld.
	self addMorphFront: aMorph.
	aMorph morphPositionInWorld: positionInWorld! !

