'From Cuis 4.2 of 25 July 2013 [latest update: #2040] on 18 July 2014 at 9:04:06.592342 am'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 7/17/2014 17:49'!
invalidateRect: localRectangle from: aMorph

	| rectInOwner rectInOwns |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	rectInOwns _ localRectangle.
	aMorph ifNotNil: [
	 	aMorph == self clippedSubmorph
			ifTrue: [ rectInOwns _ localRectangle intersect: self clippingRect ]].
	owner ifNotNil: [
		rectInOwner _ location displayBoundsOfTransformOf: rectInOwns.
		owner invalidateRect: rectInOwner ]! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 7/17/2014 17:50'!
invalidateRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self clippingRect ) ]
                ifFalse: [ super invalidateRect: damageRect from: aMorph ]
! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 7/17/2014 17:49'!
invalidateRect: localRectangle

	self invalidateRect: localRectangle from: nil! !

!methodRemoval: PluggableScrollPane #clipsSubmorphs!
PluggableScrollPane removeSelector: #clipsSubmorphs!
!methodRemoval: PasteUpMorph #invalidateRect:!
PasteUpMorph removeSelector: #invalidateRect:!
!methodRemoval: Morph #clipsSubmorphs!
Morph removeSelector: #clipsSubmorphs!
