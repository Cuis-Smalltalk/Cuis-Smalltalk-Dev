'From Cuis 4.0 of 21 April 2012 [latest update: #1271] on 7 May 2012 at 9:35:44 am'!
!classDefinition: #SystemConsistencyTest category: #'System-Tests'!
TestCase subclass: #SystemConsistencyTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Tests'!

!SystemConsistencyTest commentStamp: '<historical>' prior: 0!
General system - wide image health tests.!


!SystemConsistencyTest methodsFor: 'testing' stamp: 'jmv 5/7/2012 09:35'!
testMethodsWithUnboundGlobals
	"
	SystemConsistencyTest new testMethodsWithUnboundGlobals
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty! !


!SystemDictionary methodsFor: 'query' stamp: 'jmv 5/6/2012 19:26'!
methodsWithUnboundGlobals
	"Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty."
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
	"
	^ self allSelect: [ :m |
		m literals anySatisfy: [ :l |
			l isVariableBinding and: [
				l key isSymbol and: [
					"avoid class-side methodClass literals"
					(m methodClass bindingOf: l key)
						ifNil: [
							(Undeclared
								associationAt: l key
								ifAbsent: [ ]) ~~ l ]
						ifNotNil: [ :b |
							b ~~ l ]]]]]! !


!SystemConsistencyTest reorganize!
('testing' testMethodsWithUnboundGlobals)
!

