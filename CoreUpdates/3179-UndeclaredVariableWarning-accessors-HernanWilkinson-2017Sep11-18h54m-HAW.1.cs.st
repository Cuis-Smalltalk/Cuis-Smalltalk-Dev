'From Cuis 5.0 of 7 November 2016 [latest update: #3174] on 13 September 2017 at 3:48:54 pm'!

!UndeclaredVariableWarning methodsFor: 'accessing' stamp: 'HAW 9/13/2017 15:16:19'!
classImplementingSelector

	^class ! !

!UndeclaredVariableWarning methodsFor: 'accessing' stamp: 'HAW 9/13/2017 15:15:44'!
selector

	^selector ! !

!UndeclaredVariableWarning methodsFor: 'accessing' stamp: 'HAW 9/13/2017 15:15:34'!
variableName

	^name ! !


!UndeclaredVariableWarning reorganize!
('exceptionDescription' defaultAction)
('initialization' defaultResumeValue name:selector:class:)
('accessing' classImplementingSelector selector variableName)
!

