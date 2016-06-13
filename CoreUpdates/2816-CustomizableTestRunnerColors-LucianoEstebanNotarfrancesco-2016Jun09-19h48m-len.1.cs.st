'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 9 June 2016 at 7:59:22.65781 pm'!

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:38'!
errorColor
	^ Color red lighter! !

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:40'!
failureColor
	^ Color yellow lighter! !

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:38'!
successColor
	^ Color green lighter! !


!TestRunnerWindow methodsFor: 'updating' stamp: 'len 6/9/2016 17:40'!
updateColors
	| aTestResult theColor |
	theColor _ self widgetsColor.
	model ifNotNil: [
		model runButtonState ifFalse: [
			aTestResult _ model result.
			theColor _ aTestResult errors size + aTestResult failures size = 0
				ifTrue: [ Theme current successColor ]
				ifFalse: [
					aTestResult errors size > 0
						ifTrue: [ Theme current errorColor ]
						ifFalse: [ Theme current failureColor ]]].
		self updatePartColors: theColor ]! !

