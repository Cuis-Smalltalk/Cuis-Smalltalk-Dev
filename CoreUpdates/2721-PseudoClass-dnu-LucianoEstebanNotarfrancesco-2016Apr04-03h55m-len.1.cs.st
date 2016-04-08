'From Cuis 4.2 of 25 July 2013 [latest update: #2717] on 4 April 2016 at 3:56:16.851073 am'!

!PseudoClass methodsFor: 'methods' stamp: 'len 4/4/2016 03:55'!
removeSelectorIfInBaseSystem: selector
	self sourceCode removeKey: selector ifAbsent: [^ self].
	self organization removeElement: selector! !

