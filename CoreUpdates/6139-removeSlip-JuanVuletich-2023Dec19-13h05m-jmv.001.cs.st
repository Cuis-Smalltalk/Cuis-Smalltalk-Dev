'From Cuis6.0 [latest update: #6138] on 19 December 2023 at 1:05:28 pm'!

!Morph methodsFor: 'user interface' stamp: 'jmv 7/22/2020 23:12:08'!
flash

	self displayBounds ifNotNil: [ :r |
		Display flash: r ]! !

