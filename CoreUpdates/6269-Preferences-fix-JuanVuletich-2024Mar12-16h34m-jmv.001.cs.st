'From Cuis6.3 [latest update: #6265] on 12 March 2024 at 4:38:18 pm'!

!Preference methodsFor: 'accessing' stamp: 'jmv 3/12/2024 16:34:05'!
value: aValueOrString

	| aValue |
	aValue := aValueOrString.
	self isTypeAClass 
		ifTrue: [
			((aValueOrString isKindOf: self type) not and: [ aValueOrString isString ])
				ifTrue: [ aValue := self type readFrom: aValueOrString readStream ].
			(aValue isKindOf: self type) ifFalse: [
				self error: aValue printString, ' is not a ', type printString ].
			value := aValue ]
		ifFalse: [ "Should be handled somehow by the preference editor "
			value := aValue.
			value first = $# ifTrue: [
				value := (value withoutPrefix: '#') asSymbol ]].
	self triggerEvent: #preferenceChanged with: self.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(Preferences instanceAt: #initialFileListDirectories) name: #initialFileListDirectories description: 'Top Directories in FileList' category: #system type: #(cuis roots) value: #cuis!

