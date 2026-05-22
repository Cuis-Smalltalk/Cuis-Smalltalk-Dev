Class {
	#name : 'NegativePowerError',
	#superclass : 'ArithmeticMessageError',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionDescription' }
NegativePowerError >> defaultAction [

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	(Preferences at: #askToInstallComplexPackage) ifTrue: [
		answer := PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature requireInformFaiure: #'Complex'.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			(Preferences at: #askToInstallComplexPackage put: false)]].
	^ super defaultAction
]

{ #category : 'signaling' }
NegativePowerError >> floatErrorValue [
	^ receiver class nan
]
