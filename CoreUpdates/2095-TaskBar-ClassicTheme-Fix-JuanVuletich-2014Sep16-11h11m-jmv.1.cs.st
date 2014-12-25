'From Cuis 4.2 of 25 July 2013 [latest update: #2093] on 16 September 2014 at 11:20:21.359531 am'!

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 9/16/2014 11:19'!
magnifiedIcon
	| factor magnifiedExtent |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		factor _ extent x / icon width min: extent y / icon height.
		(factor < 1 or: [ factor > 1.7 and: [self isRoundButton]]) ifTrue: [
			magnifiedExtent _ (icon extent * factor) rounded.
			magnifiedIcon _ icon magnifyTo: magnifiedExtent ]].
	^magnifiedIcon! !

