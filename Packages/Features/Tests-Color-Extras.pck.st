'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:40:17 pm'!
'Description '!
!provides: 'Tests-Color-Extras' 1 1!
!requires: 'Color-Extras' 1 7 nil!
SystemOrganization addCategory: #'Tests-Color-Extras'!


!classDefinition: #ColorTest category: #'Tests-Color-Extras'!
TestCase subclass: #ColorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Color-Extras'!
!classDefinition: 'ColorTest class' category: #'Tests-Color-Extras'!
ColorTest class
	instanceVariableNames: ''!


!ColorTest methodsFor: 'tests' stamp: 'sqr 14/Dec/2018 20:31:38'!
testIsGrey

	self assert: Color black isGrey.
	self assert: Color grey isGrey.
	self assert: Color white isGrey.
	self deny: Color red isGrey.
	self deny: Color green isGrey.
	self deny: Color blue isGrey! !
