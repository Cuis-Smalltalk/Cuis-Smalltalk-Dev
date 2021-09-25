'From Cuis 5.0 [latest update: #4879] on 25 September 2021 at 3:39:39 pm'!

!ExtractAsParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/25/2021 15:39:23'!
askToReplaceAll
	
	| extractAll |
	
	extractAll := PopUpMenu 
		confirm: 'Do you want to extract all occurrences?' 
		trueChoice: 'Yes, extract all occurrences' 
		falseChoice: 'No, extract only the selected one'.
		
	extractAll ifFalse: [ intervals := { interval } ].! !

