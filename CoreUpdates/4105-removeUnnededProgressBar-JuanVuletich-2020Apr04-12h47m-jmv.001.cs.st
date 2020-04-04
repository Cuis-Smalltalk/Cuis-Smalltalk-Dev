'From Cuis 5.0 [latest update: #4106] on 4 April 2020 at 12:47:59 pm'!

!Dictionary methodsFor: 'removing' stamp: 'jmv 4/4/2020 12:47:43'!
unreferencedKeys
	| currentClass associations referencedAssociations |
	currentClass := nil.
	associations := self associations asIdentitySet.
	referencedAssociations := IdentitySet new: associations size.
	Smalltalk allSelect: [ :m |
		m methodClass ~~ currentClass ifTrue: [
			currentClass := m methodClass ].
		m literalsDo: [ :l |
			(l isVariableBinding and: [associations includes: l]) ifTrue: [
				referencedAssociations add: l]].
		false ].
	^((associations reject: [:assoc | referencedAssociations includes: assoc]) collect: [:assoc| assoc key]) asSet! !

