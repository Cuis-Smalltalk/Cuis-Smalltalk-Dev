'From Cuis 6.0 [latest update: #5389] on 12 July 2022 at 2:14:41 pm'!
!classDefinition: #UISupervisor category: #'System-Support'!
Object subclass: #UISupervisor
	instanceVariableNames: ''
	classVariableNames: 'UIProcess UI '
	poolDictionaries: ''
	category: 'System-Support'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:08:10'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	| previousUIProcess previousUI |
	previousUI _ UI.
	previousUIProcess _ UIProcess.
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	previousUIProcess ifNotNil: [ previousUIProcess animatedUI: nil ].
	UIProcess resume! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:10:32'!
stopUIProcess
	UIProcess ifNotNil: [
		UIProcess animatedUI: nil.
		UIProcess terminate ].
	UI _ nil .
	UIProcess _ nil.! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 7/12/2022 14:08:49'!
runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
		"remove #animatedUI:"
	
	^ process! !

!classDefinition: #UISupervisor category: #'System-Support'!
Object subclass: #UISupervisor
	instanceVariableNames: ''
	classVariableNames: 'UI UIProcess'
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.!

