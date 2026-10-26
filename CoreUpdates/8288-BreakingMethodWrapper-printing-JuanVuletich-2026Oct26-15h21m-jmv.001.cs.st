'From Cuis7.9 [latest update: #8286] on 26 October 2026 at 3:26:31 pm'!

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:24:50 pm (UTC)'!
printString

	^ self printAs: String limitedTo: 50000! !

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:25:32 pm (UTC)'!
printText

	^ self printAs: Text limitedTo: 50000! !

!methodRemoval: BreakingMethodWrapper #printTextLimitedTo: stamp: 'jmv 26/Oct/2026, 6:23:40 pm (UTC)'!
BreakingMethodWrapper removeSelector: #printTextLimitedTo:!
!methodRemoval: BreakingMethodWrapper #printStringLimitedTo: stamp: 'jmv 26/Oct/2026, 6:23:42 pm (UTC)'!
BreakingMethodWrapper removeSelector: #printStringLimitedTo:!
!methodRemoval: MethodContext #printString stamp: 'jmv 26/Oct/2026, 6:39:02 pm (UTC)'!
MethodContext removeSelector: #printString!
