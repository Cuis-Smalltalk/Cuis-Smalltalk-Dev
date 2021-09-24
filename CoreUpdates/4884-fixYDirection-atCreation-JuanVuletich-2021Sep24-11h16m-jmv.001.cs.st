'From Cuis 5.0 [latest update: #4883] on 24 September 2021 at 11:18:01 am'!

!MovableMorph methodsFor: 'initialization' stamp: 'jmv 9/24/2021 11:16:19'!
initialize
	"initialize the state of the receiver"

	super initialize.
	location _ MorphicTranslation new.
	self fixYAxisDirection.! !

