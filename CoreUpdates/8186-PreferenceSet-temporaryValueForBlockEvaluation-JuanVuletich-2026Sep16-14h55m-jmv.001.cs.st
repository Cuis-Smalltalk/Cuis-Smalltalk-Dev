'From Cuis7.9 [latest update: #8185] on 16 September 2026 at 2:55:54 pm'!

!PreferenceSet methodsFor: 'services' stamp: 'jmv 16/May/2022, 12:57:42 pm (UTC)'!
withTemporaryValue: temporaryValue of: aPreferenceName do: aBlock
	"Allows to execute a piece of code with a temporary value of the given preference. Useful for testing purposes."
	
	| currentValue |
	currentValue := self at: aPreferenceName.
	self at: aPreferenceName put: temporaryValue.
	[ aBlock value ] ensure: [ self at: aPreferenceName put: currentValue ]! !

