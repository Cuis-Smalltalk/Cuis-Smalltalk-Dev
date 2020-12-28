'From Cuis 5.0 [latest update: #4501] on 28 December 2020 at 2:58:24 pm'!

!NegativePowerError methodsFor: 'exceptionDescription' stamp: 'jmv 12/28/2020 14:58:01'!
defaultAction

	"Disable this preference to have Float nan answer (if Float receiver or argument) or Error message"
	| answer |
	Preferences askToInstallComplexPackage ifTrue: [
		answer _ PopUpMenu
			withCaption:
'Square (or even) Root of a negative Number:
Complex number support is not loaded
Would you like me to load it for you now?'
			chooseFrom: #(
				'Load Complex package'
				'Do not load Complex package'
				'Do not load Complex package and don''t ask again').
		answer = 1 ifTrue: [
			Feature require: #'Complex'.
			Smalltalk at: #Complex ifPresent: [ :cplx |
				^ (cplx basicReal: receiver imaginary: 0) perform: selector withArguments: arguments ]].
		answer = 3 ifTrue: [
			Preferences disable: #askToInstallComplexPackage ]].
	^ super defaultAction! !


!Preferences class methodsFor: 'personalization' stamp: 'jmv 12/26/2020 14:14:32'!
askToInstallComplexPackage
	^ self
		valueOfFlag: #askToInstallComplexPackage
		ifAbsent: [ true ].! !

