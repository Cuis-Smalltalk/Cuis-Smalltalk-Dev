'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 18 March 2012 at 6:35:13 pm'!

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:20'!
                  classRecategorized: aClass from: oldCategory to: newCategory

	self trigger:
		(RecategorizedEvent
			class: aClass
			category: newCategory
			oldCategory: oldCategory).

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/17/2012 23:23'!
                            selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self trigger: (RecategorizedEvent 
				selector: selector
				method: (aClass compiledMethodAt: selector ifAbsent: nil)
				protocol: newCategory
				class: aClass
				oldProtocol: oldCategory).

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }! !

