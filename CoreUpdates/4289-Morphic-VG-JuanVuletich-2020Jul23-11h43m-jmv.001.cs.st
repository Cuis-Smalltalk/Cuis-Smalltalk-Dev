'From Cuis 5.0 [latest update: #4284] on 23 July 2020 at 11:58:52 am'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 7/23/2020 11:56:45'!
invalidateDisplayRect: damageRect from: aMorph

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	aMorph ifNotNil: [
	 	aMorph == self clippedSubmorph
			ifTrue: [
				b _ self displayBounds.
				b ifNil: [ ^self ].
				clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect from: self ]! !

