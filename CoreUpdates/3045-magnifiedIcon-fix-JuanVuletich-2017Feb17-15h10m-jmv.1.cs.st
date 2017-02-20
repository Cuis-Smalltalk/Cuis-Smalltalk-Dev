'From Cuis 5.0 of 7 November 2016 [latest update: #3044] on 17 February 2017 at 3:11:08 pm'!

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 2/17/2017 15:10:48'!
magnifiedIcon
	| factor magnifiedExtent w h |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		w _ icon width.
		h _ icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * extent x / w min: 1.0 * extent y / h.
			(factor < 1 or: [ factor > 1.7 and: [self isRoundButton]]) ifTrue: [
				magnifiedExtent _ (icon extent * factor) rounded.
				magnifiedIcon _ icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

