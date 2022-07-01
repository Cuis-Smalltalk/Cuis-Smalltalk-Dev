'From Cuis 6.0 [latest update: #5341] on 30 June 2022 at 7:32:36 pm'!
!classDefinition: #Preference category: #'System-Support'!
Object subclass: #Preference
	instanceVariableNames: 'name description category value type '
	classVariableNames: 'ThePreferences '
	poolDictionaries: ''
	category: 'System-Support'!
!methodRemoval: Preference class #doesNotUnderstand: stamp: 'jmv 6/30/2022 19:32:23'!
Preference class removeSelector: #doesNotUnderstand:!
!methodRemoval: Preference class #at: stamp: 'jmv 6/30/2022 19:32:18'!
Preference class removeSelector: #at:!
!classDefinition: #Preference category: #'System-Support'!
Object subclass: #Preference
	instanceVariableNames: 'name description category value type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!Preference class reorganize!
('helpers' detectType:)
!

