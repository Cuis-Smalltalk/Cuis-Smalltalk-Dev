'From Cuis 5.0 of 7 November 2016 [latest update: #3598] on 6 February 2019 at 4:15:11 pm'!

!AutoCompleterMorph methodsFor: 'as yet unclassified' stamp: 'HAW 2/6/2019 16:14:45'!
updateColor

	| remaining alpha |

	remaining := (self timeout - self timeOfLastActivity).
	remaining < 1000 
		ifTrue: [
			alpha _ remaining / 1000.0.
			self color: (self color alpha: alpha).
			self borderColor: (borderColor alpha: alpha) ]
		ifFalse: [
			self color: self defaultColor.
			self borderColor: self defaultBorderColor ]
		
			! !

