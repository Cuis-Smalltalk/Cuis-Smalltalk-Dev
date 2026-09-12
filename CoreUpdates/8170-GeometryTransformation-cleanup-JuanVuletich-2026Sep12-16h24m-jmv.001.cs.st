'From Cuis7.9 [latest update: #8168] on 12 September 2026 at 4:34:48 pm'!

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:31:44 pm (UTC)'!
doesMirror
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	^false! !

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:33:05 pm (UTC)'!
isPureMirroring
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^ false! !


!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:31:53 pm (UTC)'!
doesMirror
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	^self subclassResponsibility! !

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:32:14 pm (UTC)'!
doesNotRotate
	"Return true if the receiver specifies no rotation."
	^self subclassResponsibility! !

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:33:12 pm (UTC)'!
isPureMirroring
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^ self subclassResponsibility! !

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 12/Sep/2026, 7:33:28 pm (UTC)'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	^self subclassResponsibility! !

