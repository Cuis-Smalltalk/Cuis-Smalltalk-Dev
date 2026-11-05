'From Cuis7.9 [latest update: #8323] on 5 November 2026 at 2:38:15 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'Squeak1.0 20/Sep/1996, 1:50:05 pm (UTC)'!
reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing
	
	LookupKey reformatAll
	"
	self selectorsDo: [:sel | self reformatMethodAt: sel].! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/Nov/2026, 5:36:00 pm (UTC)'!
reformatMethodAt: selector
	| priorMethod newCodeString method |
	priorMethod := (self compiledMethodAt: selector) shallowCopy.
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStampOrUnknown
		inFile: 2.
	SystemChangeNotifier uniqueInstance
		methodChangedFrom: priorMethod
		to: method
		selector: method selector
		inClass: method methodClass.! !

