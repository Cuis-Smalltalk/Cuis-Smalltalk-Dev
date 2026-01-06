'From Cuis7.7 [latest update: #7785] on 6 January 2026 at 2:45:30 pm'!

!Preference methodsFor: 'accessing' stamp: 'jmv 6/Jan/2026 14:08:58'!
value: aValueOrString

	| aValue |
	aValue := aValueOrString.
	self isTypeAClass 
		ifTrue: [
			((aValueOrString isKindOf: self type) not and: [ aValueOrString isString ])
				ifTrue: [
					aValue := self type readFrom: aValueOrString readStream ].
			(aValue isKindOf: self type)
				ifFalse: [
					self error: aValue printString, ' is not a ', type printString ]].
	value = aValue ifFalse: [
		value := aValue.
		self triggerEvent: #preferenceChanged with: self ].! !

