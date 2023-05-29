'From Cuis 6.0 [latest update: #5825] on 29 May 2023 at 4:17:20 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/29/2023 16:05:29'!
drawOn: aCanvas
	aCanvas drawButtonIconFromCurrentMorph ifFalse: [
		self isRoundButton
			ifTrue: [ self drawRoundLookOn: aCanvas ]
			ifFalse: [ self draw3DLookOn: aCanvas ]].! !

!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 5/29/2023 16:15:00'!
minimumExtent
	
	| unit |
	label ifNil: [
		^super minimumExtent ].

	"Assure some space for text."
	unit :=  (Preferences at: #windowTitleFont) pointSize.
	^(4 * unit) @ (3 * unit).! !

