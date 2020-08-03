'From Cuis 5.0 [latest update: #4300] on 27 July 2020 at 12:40:53 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/26/2020 20:47:43'!
restorePatch: savedPatch
	"Argument must previously be obtained by calling #savePatch:bounds:
	Senders should not assume anything about the returned object, except that it will not be nil."

self flag: #jmvHacks.
"Podriamos usar un BitBlt guardado. Este metodo termina haciendo demasiadas cosas... (innecesarias)"
	self
		image: savedPatch
		at: savedPatch offset.! !

