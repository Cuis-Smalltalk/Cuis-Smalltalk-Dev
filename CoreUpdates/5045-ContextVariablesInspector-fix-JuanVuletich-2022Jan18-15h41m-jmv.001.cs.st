'From Cuis 6.0 [latest update: #5044] on 18 January 2022 at 3:53:57 pm'!

!ContextVariablesInspector methodsFor: 'shout styling' stamp: 'jmv 1/18/2022 15:53:28'!
bindingNamesDo: aBlock
	self fieldList do: aBlock! !

!ContextVariablesInspector methodsFor: 'shout styling' stamp: 'jmv 1/18/2022 15:53:22'!
hasBindingOf: aString
	^ self fieldList includes: aString! !

